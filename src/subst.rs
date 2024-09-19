use anyhow::{anyhow, bail, Result};
use glob::glob;

use crate::parser::Variables;

fn run_subst(subst: impl AsRef<str>, variables: &Variables) -> Result<String> {
    let subst = subst.as_ref();

    if subst.starts_with("wildcard ") {
        let (_, wildcard) = subst.split_once(' ').unwrap();
        return glob(wildcard)?
            .map(|p| p.map_err(|e| anyhow!(e)).map(|p| p.display().to_string()))
            .collect::<Result<Vec<_>>>()
            .map(|v| v.join(" "));
    } else if subst.starts_with("patsubst ") {
        bail!("unimplemented: {subst}");
    } else {
        if let Some(res) = variables.get(subst) {
            Ok(res.into())
        } else {
            Ok(String::new())
        }
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
