use crate::ast::{
    Ast, Block, Expression, ExpressionId, ExpressionKind, Fn, Ident, InfixOp, Item, ItemKind,
    LiteralKind, Module, PrefixOp, Statement, StatementKind,
};
use error::{ErrorKind, Result};
use fout::source::{SourceFile, Span};
use fout::{ErrorProducer, span};

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

    current_span: Span,
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Self {
        Self {
            ast,
            source,
            depth: 0,
            locals: Vec::new(),
            expected_return_type: None,
            current_span: span!(0, 0),
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
            self.current_span = item.span;
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

        let mut return_ty_span = self.span();
        if let Some(return_ty) = &f.return_type {
            return_ty_span = return_ty.span;
            self.expected_return_type = Some(self.check_type(return_ty)?);
        }

        self.check_block(&f.body, return_ty_span)?;

        self.expected_return_type = None;

        Ok(())
    }

    pub fn check_block(&mut self, block: &'src Block<'src>, return_type_span: Span) -> Result<()> {
        self.current_span = block.span;
        self.enter_scope();

        let mut has_return = false;

        for statement in &block.statements {
            if let StatementKind::Return { .. } = &statement.kind {
                has_return = true;
            }

            self.check_statement(statement, return_type_span)?;
        }

        if !has_return {
            if let Some(expected) = self.expected_return_type {
                return Err(
                    self.error_at(ErrorKind::MissingReturnValue { expected }, return_type_span)
                );
            }
        }

        self.exit_scope();

        Ok(())
    }

    pub fn check_statement(
        &mut self,
        statement: &'src Statement<'src>,
        return_type_span: Span,
    ) -> Result<()> {
        self.current_span = statement.span;
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
                        let span = self.ast.get_expression(value).span;
                        return Err(
                            self.error_at(ErrorKind::InvalidReturnValue { expected, ty }, span)
                        );
                    }
                }
                (Some(expected), _) => {
                    return Err(
                        self.error_at(ErrorKind::MissingReturnValue { expected }, statement.span)
                    );
                }
                (_, Some(value)) => {
                    let span = self.ast.get_expression(value).span;
                    return Err(self.error_at(ErrorKind::UnexpectedReturnValue, span));
                }
                (_, _) => {}
            },
            StatementKind::IfElse { condition, then_block, else_block } => {
                let condition_ty = self.check_expression(condition)?;
                if condition_ty != Ty::Bool {
                    let span = self.ast.get_expression(condition).span;
                    return Err(self.error_at(
                        ErrorKind::ExpectedBoolInIfCondition { ty: condition_ty },
                        span,
                    ));
                }

                self.check_block(then_block, return_type_span)?;

                if let Some(else_block) = else_block {
                    self.check_block(else_block, return_type_span)?;
                }
            }
            StatementKind::Print { value } => {
                self.check_expression(value)?;
            }
            StatementKind::Loop { body } => {
                self.check_block(body, return_type_span)?;
            }
            StatementKind::While { condition, body } => {
                let condition_ty = self.check_expression(condition)?;
                if condition_ty != Ty::Bool {
                    let span = self.ast.get_expression(condition).span;
                    return Err(self.error_at(
                        ErrorKind::ExpectedBoolInWhileCondition { ty: condition_ty },
                        span,
                    ));
                }

                self.check_block(body, return_type_span)?;
            }
        }

        Ok(())
    }

    pub fn check_expression(&mut self, expr_id: &ExpressionId) -> Result<Ty> {
        let expr = self.ast.get_expression(expr_id);
        self.current_span = expr.span;

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

                return Err(self.error_at(
                    ErrorKind::UnknownVariable { ident: ident.to_string() },
                    ident.span,
                ));
            }
            ExpressionKind::UnaryOp { op, rhs } => {
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    PrefixOp::Minus => match rhs_ty {
                        Ty::Int => Ty::Int,
                        Ty::Float => Ty::Float,
                        _ => return Err(self.error_at(ErrorKind::CannotNegate, expr.span)),
                    },
                    PrefixOp::Negate => match rhs_ty {
                        Ty::Bool => Ty::Bool,
                        _ => return Err(self.error_at(ErrorKind::CannotInvert, expr.span)),
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
                                return Err(self.error_at(
                                    ErrorKind::InvalidTypeCombinationInInfixOp {
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
                            return Err(self.error_at(
                                ErrorKind::InvalidTypeCombinationInInfixOp {
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
                            return Err(self.error_at(
                                ErrorKind::InvalidTypeCombinationInInfixOp {
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
            ExpressionKind::FnCall { callee, args } => {
                assert!(
                    matches!(self.ast.get_expression(callee), Expression {
                        kind: ExpressionKind::Ident(_),
                        ..
                    }),
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
            _ => Err(self.error_at(ErrorKind::UnknownType { ident: ty.to_string() }, ty.span)),
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
    type ErrorKind = ErrorKind;

    fn name(&self) -> &str {
        "typechecker"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn span(&mut self) -> Span {
        self.current_span.clone()
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::ast::InfixOp;
    use fout::source::SourceFile;
    use fout::{Error, span};
    use parser::Parser;

    use crate::error::ErrorKind;
    use crate::{Ty, Typechecker};

    #[test]
    fn incompatible_types() {
        let source = r#"fn main() { true + 1; }
        "#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let result = typechecker.check();
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), Error {
            kind: ErrorKind::InvalidTypeCombinationInInfixOp {
                lhs: Ty::Bool,
                op: InfixOp::Add,
                rhs: Ty::Int
            },
            span: span!(12, 20)
        });
    }

    #[test]
    fn fn_return_value_expected_but_not_found() {
        let source = r#"fn a(): int {}"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(actual.unwrap_err(), Error {
            kind: ErrorKind::MissingReturnValue { expected: Ty::Int },
            span: span!(8, 11)
        })
    }

    #[test]
    fn fn_return_value_not_expected_but_found() {
        let source = r#"fn a() { return 1; }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(actual.unwrap_err(), Error {
            kind: ErrorKind::UnexpectedReturnValue,
            span: span!(16, 17)
        })
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

        assert_eq!(actual.unwrap_err(), Error {
            kind: ErrorKind::InvalidReturnValue { expected: Ty::Int, ty: Ty::Bool },
            span: span!(21, 25)
        })
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
    fn if_else_with_non_bool_condition() {
        let source = r#"fn main() { if 1 {} }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(actual.unwrap_err(), Error {
            kind: ErrorKind::ExpectedBoolInIfCondition { ty: Ty::Int },
            span: span!(15, 16)
        })
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
        let mut typechecker = Typechecker::new(&ast, &source);

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
        let mut typechecker = Typechecker::new(&ast, &source);

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
        let mut typechecker = Typechecker::new(&ast, &source);

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
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert!(actual.is_ok())
    }

    #[test]
    fn while_with_non_bool_condition() {
        let source = r#"fn main() { while 1 {} }"#;

        let source = SourceFile::new("tests".to_string(), source);
        let ast = Parser::new(&source).parse().unwrap();
        let mut typechecker = Typechecker::new(&ast, &source);

        let actual = typechecker.check();

        assert_eq!(actual.unwrap_err(), Error {
            kind: ErrorKind::ExpectedBoolInWhileCondition { ty: Ty::Int },
            span: span!(18, 19)
        })
    }
}
