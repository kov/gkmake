use std::{fs, io, path::Path};

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

    let contents = fs::read_to_string(path).into_diagnostic()?;

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

    mf.make(&goal.name).map_err(|e| miette!(e))
}
