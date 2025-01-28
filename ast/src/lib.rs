use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<'src> {
    pub mods: Vec<Mod<'src>>,

    pub(crate) expressions: HashMap<ExpressionId, Expression<'src>>,
}

impl<'src> Ast<'src> {
    pub fn new() -> Self {
        Self { mods: Vec::new(), expressions: HashMap::new() }
    }

    pub fn add_expression(&mut self, expression: Expression<'src>) -> ExpressionId {
        let id = ExpressionId(self.expressions.len());
        self.expressions.insert(id, expression);
        id
    }

    pub fn get_expression(&self, id: &ExpressionId) -> &Expression<'src> {
        self.expressions.get(id).expect("expression should exist")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Mod<'src> {
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Statement<'src> {
    Let { name: Ident<'src>, value: ExpressionId },
    Print { value: ExpressionId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpressionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'src> {
    Literal(Literal<'src>),
    Ident(Ident<'src>),
    UnaryOp { op: Operator, rhs: ExpressionId },
    BinaryOp { lhs: ExpressionId, op: Operator, rhs: ExpressionId },
    FnCall { callee: ExpressionId, args: Vec<ExpressionId> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    BitAndAssign,
    BitOrAssign,
    BitAnd,
    BitOr,
    LessThan,
    MoreThan,
    Minus,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    And,
    Or,
    LessThanEqual,
    MoreThanEqual,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal<'src> {
    Int(i64),
    Float(f64),
    Str(&'src str),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'src>(pub &'src str);

impl Deref for Ident<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
