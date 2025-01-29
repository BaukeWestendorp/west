use std::collections::HashMap;

use lexer::token::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<'src> {
    pub modules: Vec<Module<'src>>,

    pub(crate) expressions: HashMap<ExpressionId, Expression<'src>>,
}

impl<'src> Ast<'src> {
    pub fn new() -> Self {
        Self { modules: Vec::new(), expressions: HashMap::new() }
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

impl<'src> Default for Ast<'src> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'src> {
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
    Expression { expression: ExpressionId },
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
    // Prefix
    Negate,
    Invert,

    // Infix,
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

    // Postfix
    FnCall,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = match self {
            Operator::Negate => TokenKind::Minus,
            Operator::Invert => TokenKind::Bang,

            Operator::Assign => TokenKind::Eq,
            Operator::AddAssign => TokenKind::PlusEq,
            Operator::SubtractAssign => TokenKind::MinusEq,
            Operator::MultiplyAssign => TokenKind::StarEq,
            Operator::DivideAssign => TokenKind::SlashEq,
            Operator::BitAndAssign => TokenKind::AmpEq,
            Operator::BitOrAssign => TokenKind::PipeEq,
            Operator::BitAnd => TokenKind::Amp,
            Operator::BitOr => TokenKind::Pipe,
            Operator::LessThan => TokenKind::Lt,
            Operator::MoreThan => TokenKind::Gt,
            Operator::Add => TokenKind::Plus,
            Operator::Subtract => TokenKind::Minus,
            Operator::Multiply => TokenKind::Star,
            Operator::Divide => TokenKind::Slash,
            Operator::Equals => TokenKind::EqEq,
            Operator::And => TokenKind::AmpAmp,
            Operator::Or => TokenKind::PipePipe,
            Operator::LessThanEqual => TokenKind::LtEq,
            Operator::MoreThanEqual => TokenKind::GtEq,
            Operator::NotEqual => TokenKind::BangEq,

            Self::FnCall => return write!(f, "fn call"),
        };
        write!(f, "{token}")
    }
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

impl<'src> Ident<'src> {
    pub fn as_str(&self) -> &'src str {
        self.0
    }
}

impl std::fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
