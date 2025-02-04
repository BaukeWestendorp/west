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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn to_range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

#[macro_export]
macro_rules! span {
    ($start:expr, $end:expr) => {
        $crate::source::Span::new($start, $end)
    };
    ($start:expr) => {
        $crate::source::Span::new($start, $start)
    };
}
