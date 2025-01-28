use miette::NamedSource;

pub struct SourceFile<'src> {
    name: String,
    source: &'src str,
}

impl<'src> SourceFile<'src> {
    pub fn new<S: ToString>(name: S, source: &'src str) -> SourceFile<'src> {
        SourceFile { name: name.to_string(), source }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn as_str(&self) -> &'src str {
        self.source
    }
}

impl From<&SourceFile<'_>> for NamedSource<String> {
    fn from(source: &SourceFile) -> NamedSource<String> {
        NamedSource::new(source.name.clone(), source.source.to_string())
    }
}
