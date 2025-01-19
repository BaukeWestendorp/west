#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'a>(pub &'a str);

#[derive(Debug, PartialEq, Eq)]
pub struct Fn<'name> {
    pub name: Ident<'name>,
    pub params: (),
    pub body: (),
}
