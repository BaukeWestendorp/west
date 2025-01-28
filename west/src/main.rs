use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use compiler::Compiler;
use miette::{Context, Result, bail};
use parser::Parser;
use typechecker::Typechecker;
use vm::Vm;
use west_error::source::SourceFile;

/// West runner
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to run
    #[arg(short, long)]
    file: PathBuf,

    #[arg(long, default_value = "false")]
    emit_bytecode: bool,
}

fn main() -> Result<()> {
    let Args { file, emit_bytecode } = Args::parse();

    let Ok(mut source_file) = File::open(&file) else {
        bail!("file not found: {:?}", file);
    };

    let mut source = String::new();
    match source_file.read_to_string(&mut source) {
        Err(err) => bail!(err),
        _ => {}
    }

    let file_name = file.file_name().wrap_err("argument `file` is not a file.")?.to_string_lossy();
    let source = SourceFile::new(file_name, &source);

    let ast = Parser::new(&source).parse().wrap_err("failed to parse file")?;
    Typechecker::new(&ast, &source).check()?;

    let mut compiler = Compiler::new(&ast);

    let bytecode_modules = compiler.compile();

    if emit_bytecode {
        for module in &bytecode_modules {
            println!("{}", module);
        }
    }

    let mut stdout = std::io::stdout();
    Vm::new(bytecode_modules, &mut stdout).run();

    Ok(())
}
