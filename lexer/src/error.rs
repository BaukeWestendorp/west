use thiserror::Error;

pub type Result<T> = std::result::Result<T, fout::Error<ErrorKind>>;

#[derive(Error, Debug, PartialEq)]
pub enum ErrorKind {
    #[error("unknown character: '{0}'")]
    UnknownChar(char),
    #[error("unkknown keyword: '{0}'")]
    UnknownKeyword(String),
}
