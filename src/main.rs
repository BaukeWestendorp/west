use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser as ClapParser;
use west::parser::Parser;
use west::source::SourceFile;

/// West runner
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to run
    #[arg(short, long)]
    file: PathBuf,

    /// Emit bytecode
    #[arg(long, default_value = "false")]
    emit_bytecode: bool,
}

fn main() {
    let Args { file, emit_bytecode: _ } = Args::parse();

    let mut source = String::new();
    File::open(&file)
        .expect("failed to open file")
        .read_to_string(&mut source)
        .expect("failed to read file");

    let file_name = file.file_name().expect("argument `file` is not a file.");

    let source = SourceFile::new(file_name.to_string_lossy(), &source);

    let ast = match Parser::new(&source).parse() {
        Ok(ast) => ast,
        Err(errors) => {
            dbg!(errors);
            panic!("do something with errors")
        }
    };

    dbg!(ast);

    // if let Err(err) = Typechecker::new(&ast, &source).check() {
    //     panic!("failed to typecheck file: {}", err.kind.to_string());
    // }

    // let mut compiler = Compiler::new(&ast);

    // let mut bytecode_modules = compiler.compile();

    // if emit_bytecode {
    //     for module in &bytecode_modules {
    //         println!("{}", module);
    //     }
    // }

    // let mut stdout = std::io::stdout();
    // Vm::new(bytecode_modules.remove(0), &mut stdout).run();
}
