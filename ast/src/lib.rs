use std::collections::HashMap;
use std::ops::Range;

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
pub struct Item<'src> {
    pub kind: ItemKind<'src>,

    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind<'src> {
    Fn(Fn<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn<'src> {
    pub name: Ident<'src>,
    pub params: Vec<FnParam<'src>>,
    pub return_type: Option<Ident<'src>>,
    pub body: Block<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'src> {
    pub name: Ident<'src>,
    pub ty: Ident<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'src> {
    pub statements: Vec<Statement<'src>>,

    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind<'src> {
    Expression(ExpressionId),

    Let { name: Ident<'src>, value: ExpressionId },
    Return { value: Option<ExpressionId> },
    Print { value: ExpressionId },

    IfElse { condition: ExpressionId, then_block: Block<'src> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'src> {
    pub kind: StatementKind<'src>,

    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind<'src> {
    Literal(Literal<'src>),
    Ident(Ident<'src>),
    UnaryOp { op: PrefixOp, rhs: ExpressionId },
    BinaryOp { lhs: ExpressionId, op: InfixOp, rhs: ExpressionId },
    FnCall { callee: ExpressionId, args: Vec<ExpressionId> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpressionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src> {
    pub kind: ExpressionKind<'src>,

    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Prefix(PrefixOp),
    Infix(InfixOp),
    Postfix(PostfixOp),
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Prefix(op) => write!(f, "{op}"),
            Op::Infix(op) => write!(f, "{op}"),
            Op::Postfix(op) => write!(f, "{op}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Minus,
    Negate,
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = match self {
            PrefixOp::Minus => TokenKind::Minus,
            PrefixOp::Negate => TokenKind::Bang,
        };
        write!(f, "{token}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
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
}

impl std::fmt::Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = match self {
            Self::Assign => TokenKind::Eq,
            Self::AddAssign => TokenKind::PlusEq,
            Self::SubtractAssign => TokenKind::MinusEq,
            Self::MultiplyAssign => TokenKind::StarEq,
            Self::DivideAssign => TokenKind::SlashEq,
            Self::BitAndAssign => TokenKind::AmpEq,
            Self::BitOrAssign => TokenKind::PipeEq,
            Self::BitAnd => TokenKind::Amp,
            Self::BitOr => TokenKind::Pipe,
            Self::LessThan => TokenKind::Lt,
            Self::MoreThan => TokenKind::Gt,
            Self::Add => TokenKind::Plus,
            Self::Subtract => TokenKind::Minus,
            Self::Multiply => TokenKind::Star,
            Self::Divide => TokenKind::Slash,
            Self::Equals => TokenKind::EqEq,
            Self::And => TokenKind::AmpAmp,
            Self::Or => TokenKind::PipePipe,
            Self::LessThanEqual => TokenKind::LtEq,
            Self::MoreThanEqual => TokenKind::GtEq,
            Self::NotEqual => TokenKind::BangEq,
        };

        write!(f, "{token}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOp {
    FnCall,
}

impl std::fmt::Display for PostfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn call")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralKind<'src> {
    Int(i64),
    Float(f64),
    Str(&'src str),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'src> {
    pub kind: LiteralKind<'src>,

    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident<'src> {
    pub name: &'src str,

    pub span: Range<usize>,
}

impl<'src> Ident<'src> {
    pub fn as_str(&self) -> &'src str {
        self.name
    }
}

impl std::fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
