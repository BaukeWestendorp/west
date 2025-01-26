use std::collections::HashMap;
use std::ops::Range;

use ast::{Ast, Expression, ExpressionId, File, Item, Literal, Operator, Statement};
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

pub struct Typechecker<'src> {
    source: &'src SourceFile<'src>,
    ast: &'src Ast<'src>,

    _type_ids: HashMap<ExpressionId, Ty>,
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Self {
        Self { ast, source, _type_ids: HashMap::new() }
    }

    pub fn check(&mut self) -> Result<()> {
        for file in &self.ast.files {
            self.check_file(file)?;
        }

        Ok(())
    }

    pub fn check_file(&mut self, file: &File<'src>) -> Result<()> {
        for item in &file.items {
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
        for statement in &f.body.statements {
            self.check_statement(statement)?;
        }

        Ok(())
    }

    pub fn check_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.check_expression(expression)?;
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
            Expression::Ident(_) => todo!(),
            Expression::UnaryOp { op, rhs } => {
                let rhs_ty = self.check_expression(rhs)?;

                match op {
                    Operator::Minus => match rhs_ty {
                        Ty::Int => Ty::Int,
                        Ty::Float => Ty::Float,
                        _ => return Err(self.err_here(ErrorKind::CannotInvertSign)),
                    },
                    Operator::Negate => match rhs_ty {
                        Ty::Bool => Ty::Bool,
                        _ => return Err(self.err_here(ErrorKind::CannotNegate)),
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
                        if lhs_ty == Ty::Int && rhs_ty == Ty::Int {
                            Ty::Int
                        } else if lhs_ty == Ty::Float && rhs_ty == Ty::Float {
                            Ty::Float
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
}

impl ErrorProducer for Typechecker<'_> {
    type ErrorKind = ErrorKind;

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
