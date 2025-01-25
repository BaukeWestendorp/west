use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use miette::{Result, WrapErr};

/// West runner
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to run
    #[arg(short, long)]
    file: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let file_name = args.file.canonicalize().unwrap().to_string_lossy().to_string();

    let mut source_file = std::fs::File::open(args.file).expect("file should exist");
    let mut source = String::new();
    source_file.read_to_string(&mut source).expect("file should be readable");

    let session = parser::session::ParserSession::new(file_name, &source);
    let file = parser::Parser::new(session).parse().wrap_err("failed to parse file")?;

    let mut interpreter = interpreter::Interpreter::new();

    match interpreter.run_file(file) {
        Ok(()) => {
            eprintln!("File has been run successfully");
            Ok(())
        }
        Err(err) => Err(err),
    }
}
