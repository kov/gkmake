use std::{
    env, fs,
    io::{self, BufRead, BufReader},
    path::Path,
};

use makefile::Makefile;
use miette::{bail, miette, IntoDiagnostic, Result};
use parser::Parser;

mod makefile;
mod parser;
mod subst;

pub fn with_path(path: impl AsRef<Path>) -> Result<Makefile> {
    let path = path.as_ref();

    if !fs::exists(path).into_diagnostic()? {
        return Err(miette!(io::Error::new(
            io::ErrorKind::NotFound,
            format!("File {} not found.", path.display()),
        )));
    }

    if !path.is_file() {
        return Err(miette!(io::Error::new(
            io::ErrorKind::NotFound,
            format!("Path {} is not a file.", path.display()),
        )));
    }

    // TODO: this handles includes naively. Will make the miette spans wrong, as they
    // will point to the post-processed file and not know about includes at all. It
    // also doesn't handle nested includes.
    let mut contents = String::new();
    let file = BufReader::new(fs::File::open(path).into_diagnostic()?);
    let mut lines = file.lines();
    while let Some(line) = lines.next() {
        let line = line.into_diagnostic()?;
        if line.starts_with("include ") {
            let (_, include_path) = line.split_once(' ').unwrap();
            contents.push_str(fs::read_to_string(include_path).into_diagnostic()?.as_str());
        } else {
            contents.push_str(&line);
        }
        contents.push('\n');
    }

    // Safe to unwrap here as we already checked that the path
    // is to a file, so it must have a parent.
    Parser::new(&contents, path.parent().unwrap().to_owned()).parse()
}

pub fn new() -> Result<Makefile> {
    for path in ["GNUmakefile", "makefile", "Makefile"] {
        if let true = fs::exists(path).into_diagnostic()? {
            return with_path(path);
        }
    }

    Err(miette!(io::Error::new(
        io::ErrorKind::NotFound,
        "no makefile found in the current directory",
    )))
}

fn main() -> Result<()> {
    env_logger::init();

    let mf = new()?;
    let Some(goal) = mf.explicit.first() else {
        bail!("No targets.");
    };

    let Some(goal) = goal.targets().first() else {
        bail!("No targets.");
    };

    if let Some(user_provided) = env::args().nth(1) {
        mf.make(&user_provided).map_err(|e| miette!(e))
    } else {
        mf.make(&goal.name).map_err(|e| miette!(e))
    }
}
