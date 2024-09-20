use anyhow::{anyhow, bail, Result};
use glob::glob;
use log::trace;

use crate::parser::Variables;

fn run_subst(subst: impl AsRef<str>, variables: &Variables) -> Result<String> {
    let subst = subst.as_ref();

    for unsupported in [
        // text
        "subst",
        "patsubst",
        "strip",
        "findstring",
        "filter",
        "filter-out",
        "sort",
        "word",
        "wordlist",
        "firstword",
        "lastword",
        // filename manipulation
        "suffix ",
        "basename ",
        "addsuffix ",
        "addprefix ",
        "join ",
        "realpath ",
        "abspath ",
        // conditionals
        "if",
        "or",
        "and",
        "intcmp",
        // scripting
        "foreach",
        "file",
        "call",
        "value",
        "eval",
        "origin",
        "flavor",
        "shell",
        "guile",
        // logging / panic
        "info",
        "warn",
        "error",
    ] {
        if subst.starts_with(unsupported) {
            bail!("unimplemented: {subst}");
        }
    }

    let Some((func, params)) = subst.split_once(' ') else {
        return subst_variable(subst, variables);
    };

    // Find any nested functions or substitutions, and replace them.
    let params = find_and_perform_subst(params, variables)?;

    trace!("{subst} -> {params}");
    match func {
        "wildcard" => glob(&params)?
            .map(|p| p.map_err(|e| anyhow!(e)).map(|p| p.display().to_string()))
            .collect::<Result<Vec<_>>>()
            .map(|v| v.join(" ")),
        "dir" => Ok(params
            .split_ascii_whitespace()
            .map(|p| {
                if let Some((pos, _)) = p.char_indices().rfind(|(_, c)| *c == '/') {
                    (&p[..pos + 1]).to_string()
                } else {
                    "./".into()
                }
            })
            .collect::<Vec<_>>()
            .join(" ")),
        "notdir" => Ok(params
            .split_ascii_whitespace()
            .map(|p| {
                if p.ends_with('/') {
                    "".to_string()
                } else {
                    if let Some((pos, _)) = p.char_indices().rfind(|(_, c)| *c == '/') {
                        // We handle the case in which / is the last char above.
                        (&p[pos + 1..]).to_string()
                    } else {
                        p.into()
                    }
                }
            })
            .filter(|p| !p.is_empty())
            .collect::<Vec<_>>()
            .join(" ")),
        _ => subst_variable(subst, variables),
    }
}

fn subst_variable(subst: impl AsRef<str>, variables: &Variables) -> Result<String> {
    if let Some(res) = variables.get(subst.as_ref()) {
        find_and_perform_subst(res, variables)
    } else {
        Ok(String::new())
    }
}

pub fn find_and_perform_subst(input: impl AsRef<str>, variables: &Variables) -> Result<String> {
    let input = input.as_ref();

    let mut new_str = String::new();
    let mut chars = input.trim().chars();
    while let Some(c) = chars.next() {
        match c {
            '$' => {
                let Some(nc) = chars.next() else {
                    bail!("unexpected end of line");
                };

                let nc = match nc {
                    '$' => "$".into(),
                    '(' => {
                        let mut subst = String::new();
                        let mut nesting = 0;
                        loop {
                            if let Some(sc) = chars.next() {
                                if sc == '(' {
                                    nesting += 1;
                                } else if sc == ')' {
                                    if nesting == 0 {
                                        break run_subst(subst, variables)?;
                                    }

                                    nesting -= 1;
                                }
                                subst.push(sc);
                            } else {
                                bail!("unexpected end of line looking for closing ) in {input}")
                            }
                        }
                    }
                    nc => nc.to_string(),
                };
                new_str.push_str(&nc);
            }
            c => new_str.push(c),
        };
    }
    Ok(new_str)
}
