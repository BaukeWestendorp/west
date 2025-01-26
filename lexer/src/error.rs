use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ErrorKind {
    #[error("unknown character: '{0}'")]
    #[diagnostic(code(west::lexer::unknown_char))]
    UnknownChar(char),
}
