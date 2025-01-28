use std::collections::HashMap;

use ast::{Ast, Item, ParsedType, TypeId};
use error::ErrorKind;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use miette::Result;
use typechecker::Ty;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

pub struct Compiler<'src, 'ctx> {
    source: &'src SourceFile<'src>,

    ast: &'src Ast<'src>,
    types: &'src HashMap<TypeId, Ty>,

    ctx: &'ctx Context,
    builder: Builder<'ctx>,
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
    pub fn new(
        ast: &'src Ast<'src>,
        types: &'src HashMap<TypeId, Ty>,
        source: &'src SourceFile<'src>,
        ctx: &'ctx Context,
    ) -> Compiler<'src, 'ctx> {
        let builder = ctx.create_builder();
        Compiler { source, ast, types, ctx: &ctx, builder }
    }

    pub fn compile(mut self) -> Result<Vec<Module<'ctx>>> {
        self.ast.mods.iter().map(|m| self.compile_mod(&m)).collect()
    }

    pub fn compile_mod(&mut self, r#mod: &ast::Mod<'src>) -> Result<Module<'ctx>> {
        let module = self.ctx.create_module(&r#mod.name);

        for item in &r#mod.items {
            self.compile_item(item, &module)?;
        }

        Ok(module)
    }

    fn compile_item(&mut self, item: &Item<'src>, module: &Module<'ctx>) -> Result<()> {
        match item {
            Item::Fn(f) => self.compile_item_fn(f, module),
        }
    }

    fn compile_item_fn(&mut self, f: &ast::Fn<'src>, module: &Module<'ctx>) -> Result<()> {
        // FIXME: Actually get function parameter types
        let param_types = &[];

        let fn_type = self.get_llvm_fn_type(f.return_type.as_ref(), param_types);

        let function = module.add_function(&f.name, fn_type, None);
        let basic_block = self.ctx.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);
        self.builder.build_return(None).unwrap();

        Ok(())
    }

    fn get_llvm_fn_type(
        &self,
        ty: Option<&ParsedType<'src>>,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        match ty {
            Some(ty) => {
                let ty = self.types.get(&ty.id).expect("type should exist");
                match ty {
                    Ty::Int => self.ctx.i32_type().fn_type(param_types, false),
                    Ty::Float => self.ctx.f32_type().fn_type(param_types, false),
                    Ty::Str => todo!(),
                    Ty::Bool => todo!(),
                }
            }
            None => self.ctx.void_type().fn_type(param_types, false),
        }
    }
}

impl ErrorProducer for Compiler<'_, '_> {
    type ErrorKind = ErrorKind;

    fn name(&self) -> &str {
        "compiler"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn current_span(&mut self) -> std::ops::Range<usize> {
        todo!()
    }
}
