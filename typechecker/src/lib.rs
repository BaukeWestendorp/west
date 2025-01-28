use std::ops::Range;

use ast::{Ast, Expression, ExpressionId, Ident, Item, Literal, Module, Operator, Statement};
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
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Self {
        Self { ast, source, depth: 0, locals: Vec::new() }
    }

    pub fn check(&mut self) -> Result<()> {
        for module in &self.ast.modules {
            self.check_module(module)?;
        }

        Ok(())
    }

    pub fn check_module(&mut self, module: &Module<'src>) -> Result<()> {
        for item in &module.items {
            self.check_item(item)?;
        }

        Ok(())
    }

    pub fn check_item(&mut self, item: &Item<'src>) -> Result<()> {
        match item {
            Item::Fn(f) => self.check_fn_item(f),
        }
    }

    pub fn check_fn_item(&mut self, f: &ast::Fn<'src>) -> Result<()> {
        self.check_block(&f.body)?;

        Ok(())
    }

    pub fn check_block(&mut self, block: &ast::Block<'src>) -> Result<()> {
        self.enter_scope();
        for statement in &block.statements {
            self.check_statement(statement)?;
        }
        self.exit_scope();

        Ok(())
    }

    pub fn check_statement(&mut self, statement: &Statement<'src>) -> Result<()> {
        match statement {
            Statement::Let { name, value } => {
                let ty = self.check_expression(value)?;
                self.locals.push(Local { name: *name, ty, depth: self.depth });
            }
            Statement::Print { value } => {
                self.check_expression(value)?;
            }
        }

        Ok(())
    }

    pub fn check_expression(&mut self, expr_id: &ExpressionId) -> Result<Ty> {
        let expr = self.ast.get_expression(expr_id);

        let ty = match expr {
            Expression::Literal(literal) => match literal {
                Literal::Int(_) => Ty::Int,
                Literal::Float(_) => Ty::Float,
                Literal::Str(_) => Ty::Str,
                Literal::Bool(_) => Ty::Bool,
            },
            Expression::Ident(ident) => {
                for local in self.locals.iter().rev() {
                    if local.name == *ident {
                        return Ok(local.ty);
                    }
                }

                return Err(self.err_here(ErrorKind::UnknownVariable { ident: *ident }));
            }
            Expression::UnaryOp { op, rhs } => {
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    Operator::Negate => match rhs_ty {
                        Ty::Int => Ty::Int,
                        Ty::Float => Ty::Float,
                        _ => return Err(self.err_here(ErrorKind::CannotNegate)),
                    },
                    Operator::Invert => match rhs_ty {
                        Ty::Bool => Ty::Bool,
                        _ => return Err(self.err_here(ErrorKind::CannotInvert)),
                    },
                    // FIXME: find a way to prevent needing a unreachable case
                    _ => unreachable!(),
                }
            }
            Expression::BinaryOp { lhs, op, rhs } => {
                let lhs_ty = self.check_expression(lhs)?;
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
                        match (lhs_ty, rhs_ty) {
                            (Ty::Int, Ty::Int) => Ty::Int,
                            (Ty::Float, Ty::Float) => Ty::Float,
                            _ => {
                                return Err(self.err_here(
                                    ErrorKind::InvalidTypeCombinationInOperator {
                                        lhs: lhs_ty,
                                        op: *op,
                                        rhs: rhs_ty,
                                    },
                                ));
                            }
                        }
                    }
                    Operator::Equals
                    | Operator::NotEqual
                    | Operator::LessThan
                    | Operator::LessThanEqual
                    | Operator::MoreThan
                    | Operator::MoreThanEqual => {
                        if lhs_ty == rhs_ty {
                            Ty::Bool
                        } else {
                            return Err(self.err_here(
                                ErrorKind::InvalidTypeCombinationInOperator {
                                    lhs: lhs_ty,
                                    op: *op,
                                    rhs: rhs_ty,
                                },
                            ));
                        }
                    }
                    Operator::And | Operator::Or => {
                        if lhs_ty == Ty::Bool && rhs_ty == Ty::Bool {
                            Ty::Bool
                        } else {
                            return Err(self.err_here(
                                ErrorKind::InvalidTypeCombinationInOperator {
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
            Expression::FnCall { callee, args } => {
                let _callee_ty = self.check_expression(callee)?;

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
        // FIXME: Implement span tracking
        0..0
    }
}
