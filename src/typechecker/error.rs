use ariadne::{Label, Report, ReportKind};

use crate::{
    ast::InfixOp,
    source::{Span, Spanned},
};

use super::Ty;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum TypecheckerError {
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
    ExpectedReturnStmt,

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

impl TypecheckerError {
    pub fn code(&self) -> &'static str {
        match self {
            TypecheckerError::CannotNegate => "cannot-negate",
            TypecheckerError::CannotInvert => "cannot-invert",
            TypecheckerError::InvalidTypeCombinationInInfixOp { .. } => {
                "invalid-type-combination-in-infix-op"
            }
            TypecheckerError::UnknownVariable { .. } => "unknown-variable",
            TypecheckerError::UnknownType { .. } => "unknown-type",
            TypecheckerError::ExpectedReturnStmt => "expected-return-stmt",
            TypecheckerError::InvalidReturnValue { .. } => "invalid-return-value",
            TypecheckerError::MissingReturnValue { .. } => "missing-return-value",
            TypecheckerError::UnexpectedReturnValue => "unexpected-return-value",
            TypecheckerError::ExpectedBoolInIfCondition { .. } => "expected-bool-in-if-condition",
            TypecheckerError::ExpectedBoolInWhileCondition { .. } => {
                "expected-bool-in-while-condition"
            }
        }
    }
}

impl From<Spanned<TypecheckerError>> for Report<'_, Span> {
    fn from(error: Spanned<TypecheckerError>) -> Self {
        let report = Report::build(ReportKind::Error, error.span)
            .with_code(error.value.code())
            .with_message(error.value.to_string())
            .with_label(
                Label::new(error.span)
                    .with_message(error.value.to_string())
                    .with_color(ariadne::Color::Red),
            );

        report.finish()
    }
}
