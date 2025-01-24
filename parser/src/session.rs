pub struct ParserSession<'src> {
    pub file_name: String,
    pub source: &'src str,
}

impl<'src> ParserSession<'src> {
    pub fn new(file_name: String, source: &'src str) -> Self {
        Self { file_name, source }
    }
}
