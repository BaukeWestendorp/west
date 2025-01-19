use lexer::token::Token;
use miette::{Context, Error, Result};

use crate::session::ParserSession;

pub struct Parser<'src> {
    pub ses: ParserSession<'src>,
    pub tokens: TokenStream,
}

impl<'src> Parser<'src> {
    pub fn new(ses: ParserSession) -> Parser {
        let tokens = lexer::lex(&ses.source);
        // FIXME: We should not collect the tokens here.
        Parser { ses, tokens: TokenStream::new(tokens.collect()) }
    }

    pub fn parse(&'src mut self) -> Result<Vec<ast::Fn<'src>>> {
        let mut fns = vec![];

        while self.tokens.peek().is_some() {
            let r#fn = self.try_parse_fn()?;
            fns.push(r#fn);
        }

        Ok(fns)
    }

    fn try_parse_fn(&self) -> Result<ast::Fn> {
        let name = self.try_parse_ident()?;

        Ok(ast::Fn { name, params: (), body: () })
    }

    fn try_parse_ident(&self) -> Result<ast::Ident> {
        let token = self.tokens.peek_or_err()?;
        let ident = match token.ident() {
            Some(ident) => ident,
            None => {
                let err = Error::msg(format!("Expected an ident but found '{}'", token.kind));
                return Err(err);
            }
        };

        if ident.is_reserved(self.ses.source) {
            return Err(());
        }

        self.tokens.next();

        Ok(ident)
    }
}

struct TokenStream {
    tokens: Vec<Token>,
    ix: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, ix: 0 }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.peek_nth(0)
    }

    pub fn peek_or_err(&self) -> Result<&Token> {
        self.peek().wrap_err("End of file reached.")
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.ix + n)
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // FIXME: We probably should take the token
        //        from the Vec somehow instead of copying it.
        let token = *self.tokens.get(self.ix)?;
        self.ix += 1;
        Some(token)
    }
}
