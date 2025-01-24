mod block;
mod expression;
mod ident;
mod item;
mod literal;
mod statement;

pub mod error;
pub mod parser;
pub mod session;

pub use parser::*;

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::session::ParserSession;

    pub fn new_parser(source: &str) -> Parser {
        let session = ParserSession::new("tests".to_string(), source);
        crate::parser::Parser::new(session)
    }
}
