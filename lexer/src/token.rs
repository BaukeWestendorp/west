use std::{fmt::Display, ops::Range, str::FromStr};

use miette::{Error, Result, bail};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Range<usize>) -> Token {
        Token { kind, span }
    }

    pub fn to_source_str<'src>(&self, source: &'src str) -> &'src str {
        &source[self.span.clone()]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// An identifier.
    Ident,
    /// A reserved keyword.
    Keyword(Keyword),
    /// A literal.
    Literal(Literal),

    /// Whitespace.
    Whitespace,

    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `;
    Semi,

    /// `=`
    Equals,
    /// `&`
    And,
    /// `|`
    Or,
    /// `<`
    LessThan,
    /// `>`
    MoreThan,
    /// `!`
    Bang,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,

    /// `==`
    EqualsEquals,
    /// `&&`
    AndAnd,
    /// `||`
    OrOr,
    /// `<=`
    LessThanEquals,
    /// `>=`
    MoreThanEquals,
    /// `!=`
    BangEquals,

    /// `+=`
    PlusEquals,
    /// `-=`
    MinusEquals,
    /// `*=`
    StarEquals,
    /// `/=`
    SlashEquals,
    /// `&=`
    AndEquals,
    /// `|=`
    OrEquals,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident => write!(f, "ident"),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::Literal(l) => write!(f, "{}", l),

            TokenKind::Whitespace => write!(f, "whitespace"),

            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),

            TokenKind::Semi => write!(f, ";"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::And => write!(f, "&"),
            TokenKind::Or => write!(f, "|"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::MoreThan => write!(f, ">"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),

            TokenKind::EqualsEquals => write!(f, "=="),
            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OrOr => write!(f, "||"),
            TokenKind::LessThanEquals => write!(f, "<="),
            TokenKind::MoreThanEquals => write!(f, ">="),
            TokenKind::BangEquals => write!(f, "!="),
            TokenKind::PlusEquals => write!(f, "+="),
            TokenKind::MinusEquals => write!(f, "-="),
            TokenKind::StarEquals => write!(f, "*="),
            TokenKind::SlashEquals => write!(f, "/="),
            TokenKind::AndEquals => write!(f, "&="),
            TokenKind::OrEquals => write!(f, "|="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Fn,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
        }
    }
}

impl FromStr for Keyword {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "fn" => Ok(Keyword::Fn),
            _ => bail!("unknown keyword: {}", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    Int,
    Float,
    Str,
    Bool,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int => write!(f, "int"),
            Literal::Float => write!(f, "float"),
            Literal::Str => write!(f, "str"),
            Literal::Bool => write!(f, "bool"),
        }
    }
}
