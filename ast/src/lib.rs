#[derive(Debug, PartialEq)]
pub struct File<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item<'src> {
    Fn(Fn<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub params: (),
    pub body: Block<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'src> {
    pub statements: Vec<Statement<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'src> {
    Literal(Literal<'src>),
    Ident(Ident<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Int(i64),
    Float(f64),
    Str(&'src str),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'src>(pub &'src str);
