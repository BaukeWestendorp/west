use std::fmt::Display;
use std::ops::Range;
use std::str::FromStr;

use miette::Result;

use crate::error::ErrorKind;

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
    ParenOpen,
    /// `)`
    ParenClose,
    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `;
    Semi,

    /// `=`
    Eq,
    /// `&`
    Amp,
    /// `|`
    Pipe,
    /// `<`
    Lt,
    /// `>`
    Gt,
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
    EqEq,
    /// `&&`
    AmpAmp,
    /// `||`
    PipePipe,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,
    /// `!=`
    BangEq,

    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `&=`
    AmpEq,
    /// `|=`
    PipeEq,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident => write!(f, "ident"),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::Literal(l) => write!(f, "{}", l),

            TokenKind::Whitespace => write!(f, "whitespace"),

            TokenKind::ParenOpen => write!(f, "("),
            TokenKind::ParenClose => write!(f, ")"),
            TokenKind::BraceOpen => write!(f, "{{"),
            TokenKind::BraceClose => write!(f, "}}"),

            TokenKind::Semi => write!(f, ";"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Amp => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),

            TokenKind::EqEq => write!(f, "=="),
            TokenKind::AmpAmp => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::PlusEq => write!(f, "+="),
            TokenKind::MinusEq => write!(f, "-="),
            TokenKind::StarEq => write!(f, "*="),
            TokenKind::SlashEq => write!(f, "/="),
            TokenKind::AmpEq => write!(f, "&="),
            TokenKind::PipeEq => write!(f, "|="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Let,
    Return,
    Print,
    Loop,
    If,
    Else,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::Let => write!(f, "let"),
            Keyword::Return => write!(f, "return"),
            Keyword::Print => write!(f, "print"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
        }
    }
}

impl FromStr for Keyword {
    type Err = ErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "fn" => Ok(Keyword::Fn),
            "let" => Ok(Keyword::Let),
            "return" => Ok(Keyword::Return),
            "print" => Ok(Keyword::Print),
            "loop" => Ok(Keyword::Loop),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            _ => Err(ErrorKind::UnknownKeyword(s.to_string())),
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
