use ast::{Ident, InfixOp};
use miette::Diagnostic;
use thiserror::Error;

use crate::Ty;

#[derive(Error, Diagnostic, Debug, PartialEq, Eq)]
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

    #[error("unknown type: {ident}")]
    #[diagnostic(code(west::typechecker::unknown_type))]
    UnknownType { ident: Ident<'src> },

    #[error("expected return statement")]
    #[diagnostic(code(west::typechecker::expected_return_statement))]
    ExpectedReturnStatement,

    #[error("invalid return type: expected {expected}, got {ty}")]
    #[diagnostic(code(west::typechecker::invalid_return_type))]
    InvalidReturnValue { expected: Ty, ty: Ty },

    #[error("missing return value: expected a value of type {expected}")]
    #[diagnostic(code(west::typechecker::missing_return_value))]
    MissingReturnValue { expected: Ty },

    #[error("unexpected return value")]
    #[diagnostic(code(west::typechecker::unexpected_return_value))]
    UnexpectedReturnValue,

    #[error("expected a <bool> in condition of if statement, but found {ty}")]
    #[diagnostic(code(west::typechecker::invalid_if_condition))]
    ExpectedBoolInIfCondition { ty: Ty },
}
