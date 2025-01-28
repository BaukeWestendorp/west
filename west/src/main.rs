use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use compiler::Compiler;
use miette::{Context, Result};
use parser::Parser;
use typechecker::Typechecker;
use west_error::source::SourceFile;

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

    let source = SourceFile::new(file_name, &source);
    let ast = Parser::new(&source).parse().wrap_err("failed to parse file")?;
    Typechecker::new(&ast, &source).check()?;
    let mut compiler = Compiler::new(&ast);

    let bytecode_modules = compiler.compile();

    println!("{}", bytecode_modules[0]);

    // let mut stdout = std::io::stdout();
    // let mut vm = Vm::new(&mut stdout);
    // todo!("run bytecode module");

    Ok(())
}
