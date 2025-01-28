use std::path::PathBuf;

use clap::Parser as ClapParser;
use inkwell::targets::{InitializationConfig, Target};
use miette::Result;
use west::{generate_modules, write_bitcode_to_file};

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

    /// Output LLVM IR
    #[arg(long, default_value = "false")]
    llvm_ir_out: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // FIXME: Get target from arguments
    Target::initialize_aarch64(&InitializationConfig::default());

    let ctx = inkwell::context::Context::create();

    let modules = generate_modules(&args.file, &ctx)?;

    for module in modules {
        if args.llvm_ir_out {
            let path = args.output.with_extension("ll");
            module.print_to_file(&path).expect("should write to file");
        }

        write_bitcode_to_file(&module, &args.output)?;
    }

    Ok(())
}
