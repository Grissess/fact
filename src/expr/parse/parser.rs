use crate::Kinded;
use super::tokenizer::{self,Token,Tokenizer,TokenKind,Side,Op};
use super::ns::Namespace;
use crate::expr::rich::*;

use std::mem;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ErrorKind {
    TokenizerError,
    Unexpected,
    UnexpectedEof,
    TooManyPushBacks,
    Extension,
    Empty,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Context {
    Element,
    Parenthesized,
    Conjunction,
}

#[derive(Debug,Clone)]
pub enum Error {
    TokenizerError(tokenizer::Error),
    Unexpected{ ctx: Context, found: TokenKind },
    UnexpectedEof(Context),
    TooManyPushBacks{ incoming: Token, present: Token },
    Extension,
    Empty(Context),
}

impl Kinded for Error {
    type Kind = ErrorKind;

    fn kind(&self) -> ErrorKind {
        match self {
            &Error::TokenizerError(_) => ErrorKind::TokenizerError,
            &Error::Unexpected{..} => ErrorKind::Unexpected,
            &Error::UnexpectedEof(_) => ErrorKind::UnexpectedEof,
            &Error::TooManyPushBacks{..} => ErrorKind::TooManyPushBacks,
            &Error::Extension => ErrorKind::Extension,
            &Error::Empty(_) => ErrorKind::Empty,
        }
    }
}

impl From<tokenizer::Error> for Error {
    fn from(e: tokenizer::Error) -> Error {
        Error::TokenizerError(e)
    }
}

#[derive(Debug)]
pub struct Parser<I: Iterator<Item=char>> {
    tzr: Tokenizer<I>,
    current: Token,
    pushback: Option<Token>,
    ns: Namespace,
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn new(mut t: Tokenizer<I>) -> Result<Parser<I>, Error> {
        let cur = t.next_token()?;
        Ok(Parser {
            tzr: t,
            current: cur,
            pushback: None,
            ns: Namespace::new()
        })
    }

    pub fn namespace(&self) -> &Namespace { &self.ns }

    pub fn namespace_mut(&mut self) -> &mut Namespace { &mut self.ns }

    pub fn token(&self) -> &Token { &self.current }

    pub fn push(&mut self, t: Token) -> Result<(), Error> {
        if self.pushback.is_none() {
            self.pushback = Some(mem::replace(&mut self.current, t));
            Ok(())
        } else {
            Err(Error::TooManyPushBacks{ incoming: t, present: mem::replace(&mut self.pushback, None).unwrap() })
        }
    }

    pub fn check_eof(&self, ctx: Context) -> Result<(), Error> {
        match self.current {
            Token::Eof => Err(Error::UnexpectedEof(ctx)),
            _ => Ok(()),
        }
    }
    
    pub fn _advance(&mut self) -> Result<Token, Error> {
        Ok(mem::replace(&mut self.current, if let Some(_) = self.pushback {
            let old = mem::replace(&mut self.pushback, None);
            old.unwrap()
        } else {
            self.tzr.next_token()?
        }))
    }

    pub fn advance(&mut self) -> Result<Token, Error> {
        let res = self._advance();
        // println!("next token: {:?}", res);
        res
    }

    pub fn parse_element(&mut self) -> Result<Expr, Error> {
        self.check_eof(Context::Element)?;

        match self.current.kind() {
            TokenKind::Name => {
                let s = if let Token::Name(ref s) = self.current { s.clone() } else { unreachable!() };
                self.advance()?;
                Ok(Expr::Atom(Atom::Var(self.ns.map(&s))))
            },
            t @ TokenKind::Paren => {
                if let Token::Paren(Side::Left) = self.current {
                    self.parse_parenthesized()
                } else {
                    Err(Error::Unexpected{ ctx: Context::Element, found: t })
                }
            },
            t @ TokenKind::Op => {
                if let Token::Op(Op::Negate) = self.current {
                    self.advance()?;
                    Ok(Expr::Not(Box::new(self.parse_element()?)))
                } else {
                    Err(Error::Extension)
                }
            },
            TokenKind::Bracket => Err(Error::Extension),
            TokenKind::Brace => Err(Error::Extension),
            t => Err(Error::Unexpected{ ctx: Context::Element, found: t }),
        }
    }

    pub fn parse_parenthesized(&mut self) -> Result<Expr, Error> {
        if let Token::Paren(Side::Left) = self.current {
            ()
        } else {
            return Err(Error::Unexpected{ ctx: Context::Parenthesized, found: self.current.kind() });
        }

        self.advance()?;

        let res = self.parse_expr()?;

        if let Token::Paren(Side::Right) = self.current {
            self.advance()?;
            Ok(res)
        } else {
            Err(Error::Unexpected{ ctx: Context::Parenthesized, found: self.current.kind() })
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_equivs()
    }

    pub fn parse_equivs(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_impls()?;

        while let Token::Op(Op::Equal) = self.current {
            self.advance()?;
            expr = Expr::Equivalent(Box::new(expr), Box::new(self.parse_impls()?));
        }

        Ok(expr)
    }

    pub fn parse_impls(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_disjunct()?;

        while let Token::Op(Op::Arrow) = self.current {
            self.advance()?;
            expr = Expr::Implies(Box::new(expr), Box::new(self.parse_disjunct()?));
        }

        Ok(expr)
    }

    pub fn parse_disjunct(&mut self) -> Result<Expr, Error> {
        let mut exprs: Vec<Expr> = vec![self.parse_conjunct()?];

        while let Token::Op(Op::Or) = self.current {
            self.advance()?;
            exprs.push(self.parse_conjunct()?);
        }

        let len = exprs.len();
        if len == 1 {
            Ok(exprs.pop().unwrap())
        } else {
            Ok(Expr::Or(exprs))
        }
    }

    pub fn parse_conjunct(&mut self) -> Result<Expr, Error> {
        let mut exprs: Vec<Expr> = vec![match self.parse_element() {
            Ok(x) => x,
            Err(Error::Extension) => return Err(Error::Empty(Context::Conjunction)),
            Err(e) => return Err(e),
        }];

        while let Token::Op(Op::And) = self.current {
            self.advance()?;
            exprs.push(match self.parse_element() {
                Ok(x) => x,
                Err(Error::Extension) => break,
                Err(e) => return Err(e),
            });
        }

        let len = exprs.len();
        if len == 1 {
            Ok(exprs.pop().unwrap())
        } else {
            Ok(Expr::And(exprs))
        }
    }
}
