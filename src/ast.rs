use crate::{lexer::token::TokenKind, source::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<'src> {
    pub modules: Vec<Module<'src>>,
}

impl<'src> Ast<'src> {
    pub fn new() -> Self {
        Self { modules: Vec::new() }
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

    pub span: Span,
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

    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind<'src> {
    Expr(Expr<'src>),

    Let { name: Ident<'src>, value: Expr<'src> },
    Return { value: Option<Expr<'src>> },
    Print { value: Expr<'src> },

    Loop { body: Block<'src> },
    While { condition: Expr<'src>, body: Block<'src> },
    IfElse { condition: Expr<'src>, then_block: Block<'src>, else_block: Option<Block<'src>> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'src> {
    pub kind: StatementKind<'src>,

    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'src> {
    Literal(Literal<'src>),
    Ident(Ident<'src>),
    UnaryOp { op: PrefixOp, rhs: Box<Expr<'src>> },
    BinaryOp { lhs: Box<Expr<'src>>, op: InfixOp, rhs: Box<Expr<'src>> },
    FnCall { callee: Box<Expr<'src>>, args: Vec<Box<Expr<'src>>> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,

    pub span: Span,
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

    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident<'src> {
    pub name: &'src str,

    pub span: Span,
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
