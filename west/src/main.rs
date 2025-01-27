use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use compiler::Compiler;
use miette::{Context, Result};
use parser::Parser;
use typechecker::Typechecker;
use west_error::source::SourceFile;

/// West compiler
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to compile
    #[arg(short, long)]
    file: PathBuf,

    /// Output path
    #[arg(short, long)]
    output: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let file_name = args.file.canonicalize().unwrap().to_string_lossy().to_string();

    let mut source_file = std::fs::File::open(args.file).expect("file should exist");
    let mut source = String::new();
    source_file.read_to_string(&mut source).expect("file should be readable");

    let source = SourceFile::new(file_name, &source);
    let ast = Parser::new(&source).parse().wrap_err("failed to parse file")?;
    Typechecker::new(&ast, &source).check()?;

    let ctx = inkwell::context::Context::create();
    let mut compiler = Compiler::new(&ast, &source, &ctx);
    compiler.compile().wrap_err("failed to compile")?;

    Ok(())
}
