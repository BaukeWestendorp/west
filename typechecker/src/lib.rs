#![feature(assert_matches)]

use std::assert_matches::assert_matches;
use std::ops::Range;

use ast::{
    Ast, Block, Expression, ExpressionId, ExpressionKind, Fn, Ident, InfixOp, Item, ItemKind,
    LiteralKind, Module, PrefixOp, Statement, StatementKind,
};
use error::ErrorKind;
use miette::Result;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    Int,
    Float,
    Str,
    Bool,
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "<int>"),
            Ty::Float => write!(f, "<float>"),
            Ty::Str => write!(f, "<str>"),
            Ty::Bool => write!(f, "<bool>"),
        }
    }
}

#[derive(Debug)]
struct Local<'src> {
    name: Ident<'src>,
    ty: Ty,
    depth: usize,
}

pub struct Typechecker<'src> {
    source: &'src SourceFile<'src>,
    ast: &'src Ast<'src>,

    depth: usize,

    locals: Vec<Local<'src>>,

    expected_return_type: Option<Ty>,

    current_span: &'src Range<usize>,
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Self {
        Self {
            ast,
            source,
            depth: 0,
            locals: Vec::new(),
            expected_return_type: None,
            current_span: &(0..0),
        }
    }

    pub fn check(&mut self) -> Result<()> {
        for module in &self.ast.modules {
            self.check_module(module)?;
        }

        Ok(())
    }

    pub fn check_module(&mut self, module: &'src Module<'src>) -> Result<()> {
        for item in &module.items {
            self.current_span = &item.span;
            self.check_item(item)?;
        }

        Ok(())
    }

    pub fn check_item(&mut self, item: &'src Item<'src>) -> Result<()> {
        match &item.kind {
            ItemKind::Fn(f) => self.check_fn_item(f),
        }
    }

    pub fn check_fn_item(&mut self, f: &'src Fn<'src>) -> Result<()> {
        for param in &f.params {
            let local = Local {
                name: param.name.clone(),
                ty: self.check_type(&param.ty)?,
                depth: self.depth,
            };
            self.locals.push(local);
        }

        if let Some(return_ty) = &f.return_type {
            self.expected_return_type = Some(self.check_type(return_ty)?);
        }

        self.check_block(&f.body)?;

        self.expected_return_type = None;

        Ok(())
    }

    pub fn check_block(&mut self, block: &'src Block<'src>) -> Result<()> {
        self.current_span = &block.span;
        self.enter_scope();

        let mut has_return = false;

        for statement in &block.statements {
            if let StatementKind::Return { .. } = &statement.kind {
                has_return = true;
            }

            self.check_statement(statement)?;
        }

        if !has_return {
            if let Some(expected) = self.expected_return_type {
                return Err(self.err_here(ErrorKind::MissingReturnValue { expected }));
            }
        }

        self.exit_scope();

        Ok(())
    }

    pub fn check_statement(&mut self, statement: &'src Statement<'src>) -> Result<()> {
        self.current_span = &statement.span;
        match &statement.kind {
            StatementKind::Expression(expression) => {
                self.check_expression(expression)?;
            }
            StatementKind::Let { name, value } => {
                let ty = self.check_expression(value)?;
                self.locals.push(Local { name: name.clone(), ty, depth: self.depth });
            }
            StatementKind::Return { value } => match (self.expected_return_type, value) {
                (Some(expected), Some(value)) => {
                    let ty = self.check_expression(value)?;
                    if ty != expected {
                        return Err(self.err_here(ErrorKind::InvalidReturnValue { expected, ty }));
                    }
                }
                (Some(expected), None) => {
                    return Err(self.err_here(ErrorKind::MissingReturnValue { expected }));
                }
                (None, Some(_)) => {
                    return Err(self.err_here(ErrorKind::UnexpectedReturnValue));
                }
                (None, None) => {}
            },
            StatementKind::Print { value } => {
                self.check_expression(value)?;
            }
            StatementKind::Loop { body } => {
                self.check_block(body)?;
            }
        }

        Ok(())
    }

    pub fn check_expression(&mut self, expr_id: &ExpressionId) -> Result<Ty> {
        let expr = self.ast.get_expression(expr_id);
        self.current_span = &expr.span;

        let ty = match &expr.kind {
            ExpressionKind::Literal(literal) => match &literal.kind {
                LiteralKind::Int(_) => Ty::Int,
                LiteralKind::Float(_) => Ty::Float,
                LiteralKind::Str(_) => Ty::Str,
                LiteralKind::Bool(_) => Ty::Bool,
            },
            ExpressionKind::Ident(ident) => {
                for local in self.locals.iter().rev() {
                    if local.name.name == ident.name {
                        return Ok(local.ty);
                    }
                }

                return Err(self.err_here(ErrorKind::UnknownVariable { ident: ident.clone() }));
            }
            ExpressionKind::UnaryOp { op, rhs } => {
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    PrefixOp::Minus => match rhs_ty {
                        Ty::Int => Ty::Int,
                        Ty::Float => Ty::Float,
                        _ => return Err(self.err_here(ErrorKind::CannotNegate)),
                    },
                    PrefixOp::Negate => match rhs_ty {
                        Ty::Bool => Ty::Bool,
                        _ => return Err(self.err_here(ErrorKind::CannotInvert)),
                    },
                }
            }
            ExpressionKind::BinaryOp { lhs, op, rhs } => {
                let lhs_ty = self.check_expression(lhs)?;
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    InfixOp::Add | InfixOp::Subtract | InfixOp::Multiply | InfixOp::Divide => {
                        match (lhs_ty, rhs_ty) {
                            (Ty::Int, Ty::Int) => Ty::Int,
                            (Ty::Float, Ty::Float) => Ty::Float,
                            _ => {
                                return Err(self.err_here(
                                    ErrorKind::InvalidTypeCombinationInInfixOp {
                                        lhs: lhs_ty,
                                        op: *op,
                                        rhs: rhs_ty,
                                    },
                                ));
                            }
                        }
                    }
                    InfixOp::Equals
                    | InfixOp::NotEqual
                    | InfixOp::LessThan
                    | InfixOp::LessThanEqual
                    | InfixOp::MoreThan
                    | InfixOp::MoreThanEqual => {
                        if lhs_ty == rhs_ty {
                            Ty::Bool
                        } else {
                            return Err(self.err_here(
                                ErrorKind::InvalidTypeCombinationInInfixOp {
                                    lhs: lhs_ty,
                                    op: *op,
                                    rhs: rhs_ty,
                                },
                            ));
                        }
                    }
                    InfixOp::And | InfixOp::Or => {
                        if lhs_ty == Ty::Bool && rhs_ty == Ty::Bool {
                            Ty::Bool
                        } else {
                            return Err(self.err_here(
                                ErrorKind::InvalidTypeCombinationInInfixOp {
                                    lhs: lhs_ty,
                                    op: *op,
                                    rhs: rhs_ty,
                                },
                            ));
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ExpressionKind::FnCall { callee, args } => {
                assert_matches!(
                    self.ast.get_expression(callee),
                    Expression { kind: ExpressionKind::Ident(_), .. },
                    "invalid fn call. expected callee to be an ident"
                );

                let _arg_tys = args
                    .iter()
                    .map(|arg| self.check_expression(arg))
                    .collect::<Result<Vec<_>>>()?;

                // FIXME: Implement return types for functions
                Ty::Int
            }
        };

        Ok(ty)
    }

    fn check_type(&mut self, ty: &Ident<'src>) -> Result<Ty> {
        match ty.name {
            "int" => Ok(Ty::Int),
            "float" => Ok(Ty::Float),
            "str" => Ok(Ty::Str),
            "bool" => Ok(Ty::Bool),
            _ => Err(self.err_here(ErrorKind::UnknownType { ident: ty.clone() })),
        }
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.depth -= 1;

        self.locals.retain(|local| local.depth <= self.depth);
    }
}

impl<'src> ErrorProducer for Typechecker<'src> {
    type ErrorKind = ErrorKind<'src>;

    fn name(&self) -> &str {
        "typechecker"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn current_span(&mut self) -> Range<usize> {
        self.current_span.clone()
    }
}

#[cfg(test)]
mod tests {
    use parser::Parser;
    use west_error::source::SourceFile;

    use crate::Typechecker;

    #[test]
    fn incompatible_types() {
        let source = r#"
            fn main() {
                true + 1;
            }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let result = typechecker.check();
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "invalid type combination in operator: <bool> + <int>"
        );
    }

    #[test]
    fn fn_return_value_expected_but_not_found() {
        let source = r#"fn a(): int {}"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(
            actual.unwrap_err().to_string(),
            "missing return value: expected a value of type <int>".to_string()
        )
    }

    #[test]
    fn fn_return_value_not_expected_but_found() {
        let source = r#"fn a() { return 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(actual.unwrap_err().to_string(), "unexpected return value".to_string())
    }

    #[test]
    fn fn_return_value_expected_and_found() {
        let source = r#"fn a(): int { return 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn fn_return_value_expected_and_found_but_incompatible() {
        let source = r#"fn a(): int { return true; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(
            actual.unwrap_err().to_string(),
            "invalid return type: expected <int>, got <bool>".to_string()
        )
    }

    #[test]
    fn fn_return_value_not_expectend_and_not_found() {
        let source = r#"fn a() {}"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn r#loop() {
        let source = r#"fn main { loop {} }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }
}
