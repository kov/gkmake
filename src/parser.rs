use std::{collections::HashMap, iter::Peekable, sync::Arc};

use crate::makefile::{ExplicitRule, Makefile, PreReq, Rule, Target};
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
    Ident { with_pattern: bool, with_glob: bool },
    String,
    Colon,
    Equal,
    NewLine,
    RecipeLine,
}

#[derive(Debug)]
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
                let newline = remaining.find('\n').unwrap_or_else(|| remaining.len());

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
                // FIXME: this should actually allow almost all characters...
                'a'..='z' | 'A'..='Z' | '_' | '.' | '/' => TokenKind::Ident {
                    with_pattern: false,
                    with_glob: false,
                },
                '%' => TokenKind::Ident {
                    with_pattern: true,
                    with_glob: false,
                },
                '*' => TokenKind::Ident {
                    with_pattern: false,
                    with_glob: true,
                },
                ':' => TokenKind::Colon,
                '=' => TokenKind::Equal,
                '\n' => TokenKind::NewLine,
                '\t' => TokenKind::RecipeLine,
                c if c.is_whitespace() => continue,
                _c => {
                    return Some(Err(miette! {
                        labels = vec![LabeledSpan::at(
                            self.offset..self.offset + 1,
                            "this character"
                        )],
                        "unexpected character"
                    }
                    .with_source_code(self.source.to_string())))
                }
            };

            match kind {
                TokenKind::Ident {
                    ref mut with_pattern,
                    ref mut with_glob,
                } => {
                    let non_ident = remaining
                        .find(|c| match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '/' => false,
                            '%' => {
                                *with_pattern = true;
                                false
                            }
                            '*' => {
                                *with_glob = true;
                                false
                            }
                            _ => true,
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
        Parser {
            source,
            lexer: Lexer::new(source).peekable(),

            working_directory,

            rules: vec![],
            variables: HashMap::new(),

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
                } = before[0].kind
                else {
                    unreachable!();
                };

                for ident in before.iter().chain(after.iter()) {
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

                let deglob = |token: &Token<'a>| {
                    let TokenKind::Ident {
                        with_pattern: _,
                        with_glob,
                    } = token.kind
                    else {
                        unreachable!()
                    };
                    if with_glob {
                        if let Ok(paths) = glob(token.source) {
                            // FIXME: do we need to handle glob errors?
                            paths.flatten().map(|p| p.display().to_string()).collect()
                        } else {
                            Vec::<String>::new()
                        }
                    } else {
                        vec![token.source.to_owned()]
                    }
                };

                let rule: Arc<dyn Rule> = Arc::new(ExplicitRule {
                    targets: before
                        .iter()
                        .flat_map(deglob)
                        .map(|p| Target { name: p })
                        .collect(),
                    pre_reqs: after
                        .iter()
                        .flat_map(deglob)
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

                self.variables
                    .insert(before[0].source.to_owned(), token.source.trim().to_owned());
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
                        "unexpected token at the start of a line"
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
            },
            "target",
            0,
        );
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
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
            },
            "dep1",
            16,
        );
        expect_next(
            &mut lexer,
            TokenKind::Ident {
                with_pattern: false,
                with_glob: false,
            },
            "dep2",
            21,
        );
    }

    #[test]
    fn test_parse() {
        let parser = Parser::new("target target2: dep1 dep2\n", PathBuf::from("."));
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

        //assert_eq!(variables.len(), 1);

        //assert_eq!(
        //    variables.get("var").unwrap(),
        //    "a very complex! {value} 314 here"
        //);

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
