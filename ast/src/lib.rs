#[derive(Debug, PartialEq, Eq)]
pub struct File<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item<'src> {
    Fn(Fn<'src>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub params: (),
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {}
