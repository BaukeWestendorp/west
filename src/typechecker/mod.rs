use ariadne::Report;
use error::TypecheckerError;

use crate::{
    ast::{
        Ast, Block, Expr, ExprKind, Fn, Ident, InfixOp, Item, ItemKind, LiteralKind, Module,
        PrefixOp, Stmt, StmtKind,
    },
    source::{Span, Spanned},
    span,
};

pub mod error;

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
    ast: &'src Ast<'src>,

    depth: usize,

    locals: Vec<Local<'src>>,

    expected_return_type: Option<Ty>,

    current_span: Span,
    errors: Vec<Spanned<TypecheckerError>>,
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>) -> Self {
        Self {
            ast,
            depth: 0,
            locals: Vec::new(),
            expected_return_type: None,
            current_span: span!(0, 0),
            errors: Vec::new(),
        }
    }

    pub fn check(&mut self) -> Result<(), Vec<Report<'static, Span>>> {
        for module in &self.ast.modules {
            self.check_module(module);
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone().into_iter().map(|error| error.into()).collect());
        }

        Ok(())
    }

    pub fn check_module(&mut self, module: &'src Module<'src>) {
        for item in &module.items {
            self.current_span = item.span;
            self.check_item(item);
        }
    }

    pub fn check_item(&mut self, item: &'src Item<'src>) {
        match &item.kind {
            ItemKind::Fn(f) => self.check_fn_item(f),
        }
    }

    pub fn check_fn_item(&mut self, f: &'src Fn<'src>) {
        for param in &f.params {
            let ty = match self.check_type(&param.ty) {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let local = Local { name: param.name.clone(), ty, depth: self.depth };
            self.locals.push(local);
        }

        let mut return_ty_span = self.current_span;
        if let Some(return_ty) = &f.return_type {
            return_ty_span = return_ty.span;
            let ty = match self.check_type(return_ty) {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            self.expected_return_type = Some(ty);
        }

        self.check_block(&f.body, return_ty_span);

        self.expected_return_type = None;
    }

    pub fn check_block(&mut self, block: &'src Block<'src>, return_type_span: Span) {
        self.current_span = block.span;
        self.enter_scope();

        let mut has_return = false;

        for stmt in &block.stmts {
            if let StmtKind::Return { .. } = &stmt.kind {
                has_return = true;
            }

            self.check_stmt(stmt, return_type_span);
        }

        if !has_return {
            if let Some(expected) = self.expected_return_type {
                self.error(TypecheckerError::MissingReturnValue { expected }, return_type_span);
            }
        }

        self.exit_scope();
    }

    pub fn check_stmt(&mut self, stmt: &'src Stmt<'src>, return_type_span: Span) {
        self.current_span = stmt.span;
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.check_expr_type(expr).map_err(|err| self.errors.push(err)).ok();
            }
            StmtKind::Let { name, value } => {
                let ty = match self.check_expr_type(value) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                self.locals.push(Local { name: name.clone(), ty, depth: self.depth });
            }
            StmtKind::Return { value } => match (self.expected_return_type, value) {
                (Some(expected), Some(value)) => {
                    let ty = match self.check_expr_type(value) {
                        Ok(ty) => ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if ty != expected {
                        let span = value.span;
                        self.error(TypecheckerError::InvalidReturnValue { expected, ty }, span);
                    }
                }
                (Some(expected), _) => {
                    self.error(TypecheckerError::MissingReturnValue { expected }, stmt.span);
                }
                (_, Some(value)) => {
                    let span = value.span;
                    self.error(TypecheckerError::UnexpectedReturnValue, span);
                }
                (_, _) => {}
            },
            StmtKind::IfElse { condition, then_block, else_block } => {
                let condition_ty = match self.check_expr_type(condition) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if condition_ty != Ty::Bool {
                    let span = condition.span;
                    self.error(
                        TypecheckerError::ExpectedBoolInIfCondition { ty: condition_ty },
                        span,
                    );
                }

                self.check_block(then_block, return_type_span);

                if let Some(else_block) = else_block {
                    self.check_block(else_block, return_type_span);
                }
            }
            StmtKind::Print { value } => {
                self.check_expr_type(value).map_err(|err| self.errors.push(err)).ok();
            }
            StmtKind::Loop { body } => {
                self.check_block(body, return_type_span);
            }
            StmtKind::While { condition, body } => {
                let condition_ty = match self.check_expr_type(condition) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if condition_ty != Ty::Bool {
                    let span = condition.span;
                    self.error(
                        TypecheckerError::ExpectedBoolInWhileCondition { ty: condition_ty },
                        span,
                    );
                }

                self.check_block(body, return_type_span);
            }
        }
    }

    pub fn check_expr_type(&mut self, expr: &Expr) -> Result<Ty, Spanned<TypecheckerError>> {
        let expr = expr;
        self.current_span = expr.span;

        let ty = match &expr.kind {
            ExprKind::Literal(literal) => match &literal.kind {
                LiteralKind::Int(_) => Ty::Int,
                LiteralKind::Float(_) => Ty::Float,
                LiteralKind::Str(_) => Ty::Str,
                LiteralKind::Bool(_) => Ty::Bool,
            },
            ExprKind::Ident(ident) => {
                for local in self.locals.iter().rev() {
                    if local.name.name == ident.name {
                        return Ok(local.ty);
                    }
                }

                return Err(Spanned::new(
                    TypecheckerError::UnknownVariable { ident: ident.to_string() },
                    ident.span,
                ));
            }
            ExprKind::UnaryOp { op, rhs } => {
                let rhs_ty = self.check_expr_type(rhs)?;

                match op {
                    PrefixOp::Minus => match rhs_ty {
                        Ty::Int => Ty::Int,
                        Ty::Float => Ty::Float,
                        _ => return Err(Spanned::new(TypecheckerError::CannotNegate, expr.span)),
                    },
                    PrefixOp::Negate => match rhs_ty {
                        Ty::Bool => Ty::Bool,
                        _ => return Err(Spanned::new(TypecheckerError::CannotInvert, expr.span)),
                    },
                }
            }
            ExprKind::BinaryOp { lhs, op, rhs } => {
                let lhs_ty = self.check_expr_type(lhs)?;
                let rhs_ty = self.check_expr_type(rhs)?;

                match op {
                    InfixOp::Add | InfixOp::Subtract | InfixOp::Multiply | InfixOp::Divide => {
                        match (lhs_ty, rhs_ty) {
                            (Ty::Int, Ty::Int) => Ty::Int,
                            (Ty::Float, Ty::Float) => Ty::Float,
                            _ => {
                                return Err(Spanned::new(
                                    TypecheckerError::InvalidTypeCombinationInInfixOp {
                                        lhs: lhs_ty,
                                        op: *op,
                                        rhs: rhs_ty,
                                    },
                                    expr.span,
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
                            return Err(Spanned::new(
                                TypecheckerError::InvalidTypeCombinationInInfixOp {
                                    lhs: lhs_ty,
                                    op: *op,
                                    rhs: rhs_ty,
                                },
                                expr.span,
                            ));
                        }
                    }
                    InfixOp::And | InfixOp::Or => {
                        if lhs_ty == Ty::Bool && rhs_ty == Ty::Bool {
                            Ty::Bool
                        } else {
                            return Err(Spanned::new(
                                TypecheckerError::InvalidTypeCombinationInInfixOp {
                                    lhs: lhs_ty,
                                    op: *op,
                                    rhs: rhs_ty,
                                },
                                expr.span,
                            ));
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::FnCall { callee, args } => {
                assert!(
                    matches!(callee.as_ref(), Expr { kind: ExprKind::Ident(_), .. }),
                    "invalid fn call. expected callee to be an ident"
                );

                let _arg_tys = args
                    .iter()
                    .map(|arg| self.check_expr_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                // FIXME: Implement return types for functions
                Ty::Int
            }
        };

        Ok(ty)
    }

    fn check_type(&mut self, ty: &Ident<'src>) -> Result<Ty, Spanned<TypecheckerError>> {
        match ty.name {
            "int" => Ok(Ty::Int),
            "float" => Ok(Ty::Float),
            "str" => Ok(Ty::Str),
            "bool" => Ok(Ty::Bool),
            _ => {
                Err(Spanned::new(TypecheckerError::UnknownType { ident: ty.to_string() }, ty.span))
            }
        }
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.depth -= 1;

        self.locals.retain(|local| local.depth <= self.depth);
    }

    fn error(&mut self, error: TypecheckerError, span: Span) {
        self.errors.push(Spanned::new(error, span));
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::{
        ast::InfixOp,
        parser::Parser,
        source::{SourceFile, Spanned},
        span,
        typechecker::{Ty, Typechecker, error::TypecheckerError},
    };

    #[test]
    fn incompatible_types() {
        let source = r#"fn main() { true + 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::InvalidTypeCombinationInInfixOp {
                lhs: Ty::Bool,
                op: InfixOp::Add,
                rhs: Ty::Int
            },
            span!(12, 20)
        )]);
    }

    #[test]
    fn fn_return_value_expected_but_not_found() {
        let source = r#"fn a(): int {}"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::MissingReturnValue { expected: Ty::Int },
            span!(8, 11)
        )])
    }

    #[test]
    fn fn_return_value_not_expected_but_found() {
        let source = r#"fn a() { return 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::UnexpectedReturnValue,
            span!(16, 17)
        )])
    }

    #[test]
    fn fn_return_value_expected_and_found() {
        let source = r#"fn a(): int { return 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn fn_return_value_expected_and_found_but_incompatible() {
        let source = r#"fn a(): int { return true; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::InvalidReturnValue { expected: Ty::Int, ty: Ty::Bool },
            span!(21, 25)
        )])
    }

    #[test]
    fn fn_return_value_not_expectend_and_not_found() {
        let source = r#"fn a() {}"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn if_else_with_non_bool_condition() {
        let source = r#"fn main() { if 1 {} }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::ExpectedBoolInIfCondition { ty: Ty::Int },
            span!(15, 16)
        )])
    }

    #[test]
    fn if_else_with_bool_condition() {
        let source = r#"
            fn main() {
                if true {}
            }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn if_else_with_else_block() {
        let source = r#"
            fn main() {
                if true {} else {}
            }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn r#loop() {
        let source = r#"
            fn main() {
                loop {}
            }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn r#while() {
        let source = r#"
            fn main() {
                while true {}
            }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn while_with_non_bool_condition() {
        let source = r#"fn main() { while 1 {} }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast);

        let _ = typechecker.check();

        assert_eq!(typechecker.errors, vec![Spanned::new(
            TypecheckerError::ExpectedBoolInWhileCondition { ty: Ty::Int },
            span!(18, 19)
        )])
    }
}
