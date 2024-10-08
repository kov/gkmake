use std::{collections::HashMap, iter::Peekable, sync::Arc};

use crate::{
    makefile::{ExplicitRule, Makefile, PreReq, Rule, Target},
    subst::find_and_perform_subst,
};
use glob::glob;
use log::trace;
use miette::{miette, LabeledSpan, Result};
use std::path::PathBuf;

#[derive(Debug)]
struct Token<'a> {
    source: &'a str,
    offset: usize,
    kind: TokenKind,
}

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    Ident {
        with_pattern: bool,
        with_glob: bool,
        with_substitution: bool,
    },
    String,
    Colon,
    Equal,
    NewLine,
    RecipeLine,
}

#[derive(Clone, Debug)]
enum LexerState {
    Start,
    VariableValue,
    TargetDeps,
}

#[derive(Debug)]
struct Lexer<'a> {
    source: &'a str,
    offset: usize,

    state: LexerState,
}

pub type Variables = HashMap<String, String>;

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            source,
            offset: 0,
            state: LexerState::Start,
        }
    }
}

pub fn find_unescaped(s: &str, c: char) -> Option<usize> {
    let mut remaining = &s[..];
    let mut offset = 0;
    while remaining.len() > 1 {
        let (pos, _) = remaining.char_indices().find(|(_, rc)| *rc == c)?;

        offset += pos + 1; // +1 to account for skipping this char
        if pos == 0 || &remaining[pos - 1..pos] != "\\" {
            return Some(offset);
        }

        remaining = &remaining[pos + 1..];
    }
    None
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let start = self.offset;
            let remaining = &self.source[self.offset..];
            let mut chars = remaining.chars();

            // If we are looking for the value of a variable, treat it as a string, read to
            // end of line.
            if matches!(self.state, LexerState::VariableValue) {
                let newline = find_unescaped(remaining, '\n').unwrap_or_else(|| remaining.len());

                self.offset += newline;
                self.state = LexerState::Start;

                break Some(Ok(Token {
                    source: &self.source[start..self.offset],
                    offset: start,
                    kind: TokenKind::String,
                }));
            }

            // Adjust offset here so we can just skip characters like whitespaces.
            self.offset += 1;
            let mut kind = match chars.next()? {
                'a'..='z' | 'A'..='Z' | '_' | '.' | '/' => TokenKind::Ident {
                    with_pattern: false,
                    with_glob: false,
                    with_substitution: false,
                },
                '$' => TokenKind::Ident {
                    with_pattern: false,
                    with_glob: false,
                    with_substitution: true,
                },
                '%' => TokenKind::Ident {
                    with_pattern: true,
                    with_glob: false,
                    with_substitution: false,
                },
                '*' => TokenKind::Ident {
                    with_pattern: false,
                    with_glob: true,
                    with_substitution: false,
                },
                '#' => {
                    // Ignore the rest of the line for comments.
                    self.offset += remaining.find('\n').unwrap_or_else(|| remaining.len());
                    continue;
                }
                ':' => TokenKind::Colon,
                '=' => TokenKind::Equal,
                '\n' => TokenKind::NewLine,
                '\t' => TokenKind::RecipeLine,
                '\\' => {
                    // Ignore the next character.
                    self.offset += 1;
                    continue;
                }
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(miette! {
                        labels = vec![LabeledSpan::at(
                            self.offset - 1..self.offset,
                            "this character"
                        )],
                        "unexpected character '{c}' when starting parse pass"
                    }
                    .with_source_code(self.source.to_string())))
                }
            };

            match kind {
                TokenKind::Ident {
                    ref mut with_pattern,
                    ref mut with_glob,
                    ref mut with_substitution,
                } => {
                    // We treat substitutions as idents. Handle these first.
                    let mut include_next = false;
                    let mut bracket_depth = 0;
                    let non_ident = remaining
                        .find(|c| {
                            // We found a \, so we treat whatever comes
                            // next as part of the ident.
                            if include_next {
                                include_next = false;
                                return false;
                            }

                            match c {
                                ':' | '=' | '\n' | '\t' | ' ' => bracket_depth == 0,
                                '\\' => {
                                    include_next = true;
                                    false
                                }
                                '%' => {
                                    *with_pattern = true;
                                    false
                                }
                                '*' => {
                                    *with_glob = true;
                                    false
                                }
                                '$' => {
                                    *with_substitution = true;
                                    false
                                }
                                '(' => {
                                    bracket_depth += 1;
                                    false
                                }
                                ')' => {
                                    bracket_depth -= 1;
                                    false
                                }
                                _ => false,
                            }
                        })
                        .unwrap_or_else(|| remaining.len());

                    self.offset = start + non_ident;
                }
                TokenKind::Colon => self.state = LexerState::TargetDeps,
                TokenKind::Equal => self.state = LexerState::VariableValue,
                TokenKind::NewLine => self.state = LexerState::Start,
                TokenKind::RecipeLine => {
                    let newline = remaining.find('\n').unwrap_or_else(|| remaining.len());

                    self.offset = start + newline;
                }
                TokenKind::String => unreachable!(),
            };

            // Empty token?
            if start == self.offset {
                return Some(Err(miette! {
                    labels = vec![LabeledSpan::at(
                        start - 1..start,
                        "here"
                    )],
                    "generated empty token"
                }
                .with_source_code(self.source.to_string())));
            }

            break Some(Ok(Token {
                source: &self.source[start..self.offset],
                offset: start,
                kind,
            }));
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    source: &'a str,
    lexer: Peekable<Lexer<'a>>,

    working_directory: PathBuf,

    rules: Vec<Arc<dyn Rule>>,
    variables: Variables,

    current_rule: Option<Arc<dyn Rule>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, working_directory: PathBuf) -> Self {
        let mut variables = HashMap::new();

        // Load variables with the automatically provided ones.
        variables.insert(
            "srcdir".to_string(),
            working_directory.display().to_string(),
        );

        Parser {
            source,
            lexer: Lexer::new(source).peekable(),

            working_directory,

            rules: vec![],
            variables,

            current_rule: None,
        }
    }

    fn parse_toplevel(&mut self, token: Token<'a>) -> Result<()> {
        trace!("Parsing toplevel, starting with {token:?}");

        // Consume all ident tokens, until we find some other kind of token.
        let mut before = vec![];
        before.push(token);
        while let Some(Ok(Token {
            kind: TokenKind::Ident { .. },
            ..
        })) = self.lexer.peek()
        {
            let token = self.lexer.next().unwrap().unwrap();
            trace!("Found ident token: {token:?}");
            before.push(token);
        }

        trace!("Finished consuming idents at {:?}", self.lexer.peek());

        // Perform a bunch of validations...
        let Some(Ok(token)) = self.lexer.next() else {
            return Err(miette! {
                labels = vec![LabeledSpan::at(
                    self.source.len() - 1..self.source.len(),
                    "here"
                )],
                "unexpected end of file looking for operator"
            }
            .with_source_code(self.source.to_string()));
        };

        if matches!(token.kind, TokenKind::Equal) {
            if before.len() > 1 {
                let offending = &before[1];
                return Err(miette! {
                    labels = vec![LabeledSpan::at(
                        offending.offset..offending.offset + offending.source.len(),
                        "this name"
                    )],
                    "assigning multiple variables is not supported"
                }
                .with_source_code(self.source.to_string()));
            }
        }

        if !matches!(token.kind, TokenKind::Colon | TokenKind::Equal) {
            return Err(miette! {
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.source.len(),
                    "this token"
                )],
                "unexpected token, expected : or ="
            }
            .with_source_code(self.source.to_string()));
        };

        // We found a new toplevel. Finalize any pending rule.
        if let Some(rule) = self.current_rule.take() {
            self.rules.push(rule);
        }

        // Colon tokens may be simple, :=, ::=. The latter 2 are actually assignment operators.
        let mut is_simple_assignment = false;
        let token = if token.kind == TokenKind::Colon {
            let mut token = token;
            if let Some(Ok(Token {
                kind: TokenKind::Colon | TokenKind::Equal,
                ..
            })) = self.lexer.peek()
            {
                token = self.lexer.next().unwrap().unwrap();

                if token.kind == TokenKind::Equal {
                    is_simple_assignment = true;
                }
            }
            if let Some(Ok(Token {
                kind: TokenKind::Equal,
                ..
            })) = self.lexer.peek()
            {
                // This is a := or ::= assignment, we'll need to do
                // substitution on this variable immediately.
                is_simple_assignment = true;
                token = self.lexer.next().unwrap().unwrap();
            }
            token
        } else {
            token
        };

        // We have an operator token, we know enough to categorize this line,
        // so now we can finalize our parsing.
        match token.kind {
            TokenKind::Colon => {
                let mut after = vec![];
                while let Some(Ok(Token {
                    kind: TokenKind::Ident { .. },
                    ..
                })) = self.lexer.peek()
                {
                    after.push(self.lexer.next().unwrap().unwrap());
                }

                // Validate targets and depdendencies. Identify the type of rule from the first target.
                let TokenKind::Ident {
                    with_pattern,
                    with_glob: _,
                    with_substitution: _,
                } = before[0].kind
                else {
                    unreachable!();
                };

                for ident in before.iter() {
                    let TokenKind::Ident {
                        with_pattern: ident_with_pattern,
                        ..
                    } = ident.kind
                    else {
                        unreachable!();
                    };

                    if ident_with_pattern != with_pattern {
                        return Err(miette! {
                            labels = vec![LabeledSpan::at(
                                ident.offset..ident.offset + ident.source.len(),
                                "here"
                            )],
                            "mixing pattern with explicit rule"
                        }
                        .with_source_code(self.source.to_string()));
                    }
                }

                let subst_or_deglob = |token: &Token<'a>| -> Result<Vec<String>> {
                    let TokenKind::Ident {
                        with_pattern: _,
                        with_glob,
                        with_substitution,
                    } = token.kind
                    else {
                        unreachable!()
                    };
                    if with_substitution {
                        find_and_perform_subst(token.source, &self.variables)
                            .map(|s| s.split_ascii_whitespace().map(|s| s.to_owned()).collect())
                            .map_err(|e| miette!(e))
                    } else if with_glob {
                        if let Ok(paths) = glob(token.source) {
                            paths
                                .map(|p| p.map_err(|e| miette!(e)).map(|p| p.display().to_string()))
                                .collect::<Result<Vec<_>>>()
                        } else {
                            Ok(Vec::<String>::new())
                        }
                    } else {
                        Ok(vec![token.source.to_owned()])
                    }
                };

                // FIXME: ugh.
                let before: Vec<String> = before
                    .iter()
                    .map(subst_or_deglob)
                    .collect::<Result<Vec<Vec<String>>>>()?
                    .into_iter()
                    .flatten()
                    .collect();
                let after: Vec<String> = after
                    .iter()
                    .map(subst_or_deglob)
                    .collect::<Result<Vec<Vec<String>>>>()?
                    .into_iter()
                    .flatten()
                    .collect();

                let rule: Arc<dyn Rule> = Arc::new(ExplicitRule {
                    targets: before.into_iter().map(|p| Target { name: p }).collect(),
                    pre_reqs: after
                        .into_iter()
                        .map(|p| PreReq {
                            name: p,
                            order_only: false,
                        })
                        .collect(),
                    recipe: vec![].into(),
                });

                self.current_rule = Some(rule);
            }
            TokenKind::Equal => {
                let Some(Ok(token)) = self.lexer.next() else {
                    return Err(miette! {
                        labels = vec![LabeledSpan::at(
                            self.source.len() - 1..self.source.len(),
                            "here"
                        )],
                        "unexpected end of file looking for operator"
                    }
                    .with_source_code(self.source.to_string()));
                };

                if !matches!(token.kind, TokenKind::String) {
                    return Err(miette! {
                        labels = vec![LabeledSpan::at(
                            token.offset..token.offset + token.source.len(),
                            "this token"
                        )],
                        "unexpected token, expected string"
                    }
                    .with_source_code(self.source.to_string()));
                }

                // We may have eaten some escaped newlines, replace them with a space.
                let value = token.source.trim().replace("\\\n", " ");

                // Simple assignment variables are expected to have their
                // value substituted a single time, on declaration.
                let value = if is_simple_assignment {
                    find_and_perform_subst(value, &self.variables).map_err(|e| miette!(e))?
                } else {
                    value
                };

                self.variables.insert(before[0].source.to_owned(), value);
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    pub fn parse(mut self) -> Result<Makefile> {
        while let Some(token) = self.lexer.next() {
            let token = token?;
            let kind = token.kind.clone();

            match kind {
                TokenKind::NewLine => continue,
                TokenKind::Ident { .. } => self.parse_toplevel(token)?,
                TokenKind::RecipeLine => {
                    let Some(ref mut rule) = self.current_rule else {
                        return Err(miette! {
                            labels = vec![LabeledSpan::at(
                                token.offset..token.offset + token.source.len(),
                                "here"
                            )],
                            "recipe started with no associated target"
                        }
                        .with_source_code(self.source.to_string()));
                    };

                    rule.push_to_recipe(token.source);
                }
                _ => {
                    return Err(miette! {
                        labels = vec![LabeledSpan::at(
                            token.offset..token.offset + token.source.len(),
                            "this token"
                        )],
                        "unexpected token"
                    }
                    .with_source_code(self.source.to_string()));
                }
            }
        }

        if self.current_rule.is_some() {
            self.rules.push(self.current_rule.take().unwrap());
        }

        Ok(Makefile {
            working_directory: self.working_directory,
            explicit: self.rules,
            variables: self.variables,
        })
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use test_log::test;

    use super::*;

    fn expect_next(
        lexer: &mut Lexer,
        expected_kind: TokenKind,
        expected_source: &str,
        expected_offset: usize,
    ) {
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token {source, offset, kind}))
                if kind == expected_kind && source == expected_source && offset == expected_offset
        ));
    }

    #[test]
    fn test_lex_target_line() {
        let mut lexer = Lexer::new("target target2: dep1 dep2\n");
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
                with_substitution: false,
            },
            "target",
            0,
        );
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
                with_substitution: false,
            },
            "target2",
            7,
        );
        expect_next(&mut lexer, TokenKind::Colon, ":", 14);
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
                with_substitution: false,
            },
            "dep1",
            16,
        );
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
                with_substitution: false,
            },
            "dep2",
            21,
        );
    }

    #[test]
    fn test_substitution_on_targets() {
        let parser = Parser::new(
            concat!(
                "PARSER = $(srcdir)/src/parser.rs\n",
                "$(wildcard src/m*.rs): $(wildcard src/s*.rs) src/parser.rs\n",
                "$(srcdir)/src/main.rs: $(PARSER)\n"
            ),
            PathBuf::from("."),
        );
        let mf = parser.parse().expect("Failed to parse");

        let expected = indoc! {"
            [Rule(
                targets = [Target { name: \"src/main.rs\" }, Target { name: \"src/makefile.rs\" }]
                pre_reqs = [PreReq { name: \"src/subst.rs\", order_only: false }, PreReq { name: \"src/parser.rs\", order_only: false }]
                recipe = RefCell { value: [] }
            ), Rule(
                targets = [Target { name: \"./src/main.rs\" }]
                pre_reqs = [PreReq { name: \"./src/parser.rs\", order_only: false }]
                recipe = RefCell { value: [] }
            )]"
        };

        assert_eq!(expected, format!("{:?}", mf.explicit));
    }

    #[test]
    fn test_parse() {
        let parser = Parser::new(
            "# Simple makefile\ntarget target2: dep1 dep2\n",
            PathBuf::from("."),
        );
        let mf = parser.parse().expect("Failed to parse");

        let expected = indoc! {"
            [Rule(
                targets = [Target { name: \"target\" }, Target { name: \"target2\" }]
                pre_reqs = [PreReq { name: \"dep1\", order_only: false }, PreReq { name: \"dep2\", order_only: false }]
                recipe = RefCell { value: [] }
            )]"
        };

        assert_eq!(expected, format!("{:?}", mf.explicit));

        let parser = Parser::new(
            "var = a very complex! {value} 314 here\n",
            PathBuf::from("."),
        );
        let mf = parser.parse().expect("Failed to parse");

        assert_eq!(mf.variables.len(), 2); // 2 because we now have our first make-supplied one

        assert_eq!(
            mf.variables.get("var").unwrap(),
            "a very complex! {value} 314 here"
        );

        assert_eq!(mf.explicit.len(), 0);
    }

    #[test]
    fn test_parse_glob() {
        let parser = Parser::new("src/m*.rs: src/p*.rs Cargo.toml\n", PathBuf::from("."));
        let mf = parser.parse().expect("Failed to parse");

        let expected = indoc! {"
            [Rule(
                targets = [Target { name: \"src/main.rs\" }, Target { name: \"src/makefile.rs\" }]
                pre_reqs = [PreReq { name: \"src/parser.rs\", order_only: false }, PreReq { name: \"Cargo.toml\", order_only: false }]
                recipe = RefCell { value: [] }
            )]"
        };

        assert_eq!(expected, format!("{:?}", mf.explicit));
    }

    #[test]
    fn test_mixing_rule_types() {
        let parser = Parser::new("target %.o: dep %.c\n", PathBuf::from("."));
        let res = parser.parse();

        assert!(res.is_err());
        assert_eq!(
            res.unwrap_err().to_string(),
            "mixing pattern with explicit rule"
        );

        let parser = Parser::new("src/*.rs %.o : dep %.c\n", PathBuf::from("."));
        let res = parser.parse();

        assert!(res.is_err());
        assert_eq!(
            res.unwrap_err().to_string(),
            "mixing pattern with explicit rule"
        );
    }
}
