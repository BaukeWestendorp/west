use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use miette::Result;

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
    let mut compiler = compiler::Compiler::new(session);

    let chunk = compiler.compile()?;

    let mut vm = vm::Vm::new();
    vm.push_chunk(chunk.clone());
    vm.run();

    Ok(())
}
