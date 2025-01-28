use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use ast::{Ast, TypeId};
use compiler::Compiler;
use inkwell::context::Context;
use inkwell::module::Module;
use miette::{Context as _, Result};
use parser::Parser;
use typechecker::{Ty, Typechecker};
use west_error::source::SourceFile;

pub fn generate_modules<'ctx>(input_path: &Path, ctx: &'ctx Context) -> Result<Vec<Module<'ctx>>> {
    let mut source = String::new();
    File::open(input_path)
        .expect("file should exist")
        .read_to_string(&mut source)
        .expect("file should be readable");

    let file_name = input_path
        .canonicalize()
        .expect("should be able to canonicalize file")
        .to_string_lossy()
        .to_string();

    let source = SourceFile::new(file_name, &source);

    let ast = generate_ast(&source)?;

    let types = typecheck(&ast, &source)?;

    let compiler = Compiler::new(&ast, &types, &source, &ctx);
    let modules = compiler.compile().wrap_err("failed to compile")?;

    Ok(modules)
}

pub fn write_bitcode_to_file<'ctx>(module: &Module<'ctx>, output_path: &Path) -> Result<()> {
    if !module.write_bitcode_to_path(output_path) {
        miette::bail!("failed to write bitcode to file");
    }

    Ok(())
}

pub fn generate_ast<'src>(source: &'src SourceFile<'src>) -> Result<Ast<'src>> {
    Parser::new(&source).parse().wrap_err("failed to parse file")
}

pub fn typecheck<'src>(
    ast: &'src Ast<'src>,
    source: &'src SourceFile<'src>,
) -> Result<HashMap<TypeId, Ty>> {
    Typechecker::new(&ast, &source).check()
}
