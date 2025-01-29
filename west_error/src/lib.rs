use std::ops::Range;

use miette::{LabeledSpan, MietteDiagnostic, NamedSource, SourceSpan};
use source::SourceFile;

pub mod source;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<K: PartialEq> {
    pub kind: K,
    pub source: NamedSource<String>,
    pub span: SourceSpan,
}

pub trait ErrorProducer {
    type ErrorKind: ToString;

    fn name(&self) -> &str;

    fn source(&self) -> &SourceFile;

    fn current_span(&mut self) -> Range<usize>;

    fn err_here(&mut self, kind: Self::ErrorKind) -> miette::Error {
        miette::Error::from(
            MietteDiagnostic::new(kind.to_string())
                .with_label(LabeledSpan::at(self.current_span(), "here")),
        )
        .with_source_code(NamedSource::from(self.source()))
    }
}
