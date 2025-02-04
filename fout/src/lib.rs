use source::{SourceFile, Span};

pub mod source;

pub type Result<T, K> = std::result::Result<T, Error<K>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<K: PartialEq> {
    pub kind: K,
    pub span: Span,
}

pub trait ErrorProducer {
    type ErrorKind: PartialEq;

    fn name(&self) -> &str;

    fn source(&self) -> &SourceFile;

    fn span(&mut self) -> Span;

    fn error_here(&mut self, kind: Self::ErrorKind) -> Error<Self::ErrorKind> {
        let span = self.span();
        self.error_at(kind, span)
    }

    fn error_at(&mut self, kind: Self::ErrorKind, span: Span) -> Error<Self::ErrorKind> {
        Error { kind, span }
    }
}
