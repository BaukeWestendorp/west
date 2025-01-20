pub struct ParserSession<'src> {
    pub source: &'src str,
}

impl<'src> ParserSession<'src> {
    pub fn new(source: &'src str) -> Self {
        Self { source }
    }
}
