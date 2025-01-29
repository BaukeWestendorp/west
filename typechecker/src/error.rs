use ast::{Ident, InfixOp};
use miette::Diagnostic;
use thiserror::Error;

use crate::Ty;

#[derive(Error, Diagnostic, Debug)]
pub enum ErrorKind<'src> {
    #[error("cannot invert sign")]
    #[diagnostic(code(west::typechecker::cannot_invert_sign))]
    CannotNegate,

    #[error("cannot invert conditional")]
    #[diagnostic(code(west::typechecker::cannot_negate))]
    CannotInvert,

    #[error("invalid type combination in operator: {lhs} {op} {rhs}")]
    #[diagnostic(code(west::typechecker::invalid_type_combination_in_operator))]
    InvalidTypeCombinationInInfixOp { lhs: Ty, op: InfixOp, rhs: Ty },

    #[error("unknown variable: {ident}")]
    #[diagnostic(code(west::typechecker::unknown_variable))]
    UnknownVariable { ident: Ident<'src> },
}
