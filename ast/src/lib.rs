use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Ast<'src> {
    pub files: Vec<File<'src>>,

    pub(crate) expressions: HashMap<ExpressionId, Expression<'src>>,
}

impl<'src> Ast<'src> {
    pub fn new() -> Self {
        Self { files: Vec::new(), expressions: HashMap::new() }
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

#[derive(Debug, PartialEq)]
pub struct File<'src> {
    pub items: Vec<Item<'src>>,
}

#[derive(Debug, PartialEq)]
pub enum Item<'src> {
    Fn(Fn<'src>),
}

#[derive(Debug, PartialEq)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub params: (),
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(ExpressionId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpressionId(pub usize);

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal<'src> {
    Int(i64),
    Float(f64),
    Str(&'src str),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'src>(pub &'src str);
