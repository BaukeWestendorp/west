use crate::ast::InfixOp;
use thiserror::Error;

use crate::Ty;

pub type Result<T> = std::result::Result<T, fout::Error<ErrorKind>>;

#[derive(Error, Debug, PartialEq)]
pub enum ErrorKind {
    #[error("cannot invert sign")]
    CannotNegate,

    #[error("cannot invert conditional")]
    CannotInvert,

    #[error("invalid type combination in operator: {lhs} {op} {rhs}")]
    InvalidTypeCombinationInInfixOp { lhs: Ty, op: InfixOp, rhs: Ty },

    #[error("unknown variable: {ident}")]
    UnknownVariable { ident: String },

    #[error("unknown type: {ident}")]
    UnknownType { ident: String },

    #[error("expected return statement")]
    ExpectedReturnStatement,

    #[error("invalid return type: expected {expected}, got {ty}")]
    InvalidReturnValue { expected: Ty, ty: Ty },

    #[error("missing return value: expected a value of type {expected}")]
    MissingReturnValue { expected: Ty },

    #[error("unexpected return value")]
    UnexpectedReturnValue,

    #[error("expected a <bool> in condition of if statement, but found {ty}")]
    ExpectedBoolInIfCondition { ty: Ty },

    #[error("expected a <bool> in condition of while statement, but found {ty}")]
    ExpectedBoolInWhileCondition { ty: Ty },
}
