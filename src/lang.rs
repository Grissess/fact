use super::expr::{simple,rich};
use super::expr::parse::{parser,tokenizer};
use super::expr::parse::tokenizer::{Token,TokenKind,Side,Op};
use super::expr::parse::ns::Namespace;
use super::expr::nf::{NormalForm,NormalFormKind,NormalFormVec};

use std::collections::HashMap;
use std::fmt;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ValueKind {
    Simple,
    Rich,
    Bool,
    NormalForm,
}

use crate::Kinded;

#[derive(Debug,Clone)]
pub enum Value {
    Simple(simple::Expr),
    Rich(rich::Expr),
    Bool(bool),
    NormalForm(NormalForm),
}

impl Kinded for Value {
    type Kind = ValueKind;

    fn kind(&self) -> ValueKind {
        match self {
            &Value::Simple(_) => ValueKind::Simple,
            &Value::Rich(_) => ValueKind::Rich,
            &Value::Bool(_) => ValueKind::Bool,
            &Value::NormalForm(_) => ValueKind::NormalForm,
        }
    }
}

impl From<simple::EvalResult> for Value {
    fn from(e: simple::EvalResult) -> Value {
        match e {
            simple::EvalResult::Const(b) => Value::Bool(b),
            simple::EvalResult::Expr(x) => Value::Simple(x),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Simple(x) => write!(f, "{}", x),
            Value::Rich(x) => write!(f, "{}", x),
            Value::Bool(b) => write!(f, "{}", b),
            Value::NormalForm(n) => write!(f, "{:?}", n),
        }
    }
}

impl Value {
    pub fn convert(self, kind: ValueKind, ctx: Context) -> Result<Value, (Error, Value)> {
        match kind {
            ValueKind::Simple => match self {
                Value::Simple(x) => Ok(Value::Simple(x)),
                Value::Rich(x) => Ok(Value::Simple(x.clone().into_simple().map_err(|e| (e.into(), Value::Rich(x)))?)),
                Value::NormalForm(n) => Ok(Value::Simple(n.clone().into_simple().map_err(|e| (e.into(), Value::NormalForm(n)))?)),
                x => Err((Error::Inconvertible{ found: x.kind(), expected: kind, context: ctx }, x)),
            },
            ValueKind::Rich => match self {
                Value::Rich(x) => Ok(Value::Rich(x)),
                x => Err((Error::Inconvertible{ found: x.kind(), expected: kind, context: ctx }, x)),
            },
            ValueKind::Bool => match self {
                Value::Bool(b) => Ok(Value::Bool(b)),
                n @ Value::Simple(_) | n @ Value::Rich(_) | n @ Value::NormalForm(_) => {
                    let x = match n {
                        Value::Simple(x) => x,
                        Value::Rich(x) => x.clone().into_simple().map_err(|e| (e.into(), Value::Rich(x)))?,
                        Value::NormalForm(n) => n.clone().into_simple().map_err(|e| (e.into(), Value::NormalForm(n)))?,
                        _ => unreachable!(),
                    };
                    match x.simplify_tauto() {
                        simple::EvalResult::Const(b) => Ok(Value::Bool(b)),
                        simple::EvalResult::Expr(x) => Err((Error::Unsimplifiable{ context: ctx }, Value::Simple(x))),
                    }
                },
                x => Err((Error::Inconvertible{ found: x.kind(), expected: kind, context: ctx }, x)),
            },
            ValueKind::NormalForm => match self {
                Value::NormalForm(n) => Ok(Value::NormalForm(n)),
                x => Err((Error::NotUniquelyConvertible{ found: x.kind(), expected: kind, context: ctx }, x)),
            },
        }
    }
}

#[derive(Debug)]
pub struct Pretty<'a, 'b>(&'a Value, &'b Namespace);

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ErrorKind {
    ParserError,
    SimpleExprError,
    BadValueKind,
    Unexpected,
    Inconvertible,
    NotUniquelyConvertible,
    Unsimplifiable,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Context {
    Rewrite,
    Statement,
}

#[derive(Debug)]
pub enum Error {
    ParserError(parser::Error),
    SimpleExprError(simple::Error),
    BadValueKind{ found: ValueKind, expected: ValueKind, context: Context },
    Unexpected{ found: TokenKind, expected: TokenKind, context: Context },
    Inconvertible{ found: ValueKind, expected: ValueKind, context: Context },
    NotUniquelyConvertible{ found: ValueKind, expected: ValueKind, context: Context },
    Unsimplifiable{ context: Context },
}

impl Kinded for Error {
    type Kind = ErrorKind;

    fn kind(&self) -> ErrorKind {
        match self {
            &Error::ParserError(_) => ErrorKind::ParserError,
            &Error::SimpleExprError(_) => ErrorKind::SimpleExprError,
            &Error::BadValueKind{..} => ErrorKind::BadValueKind,
            &Error::Unexpected{..} => ErrorKind::Unexpected,
            &Error::Inconvertible{..} => ErrorKind::Inconvertible,
            &Error::NotUniquelyConvertible{..} => ErrorKind::NotUniquelyConvertible,
            &Error::Unsimplifiable{..} => ErrorKind::Unsimplifiable,
        }
    }
}

impl From<parser::Error> for Error {
    fn from(e: parser::Error) -> Error {
        Error::ParserError(e)
    }
}

impl From<simple::Error> for Error {
    fn from(e: simple::Error) -> Error {
        Error::SimpleExprError(e)
    }
}

#[derive(Debug)]
pub struct Interpreter<I: Iterator<Item=char>> {
    vars: HashMap<String, Value>,
    parser: parser::Parser<I>,
}

impl<I: Iterator<Item=char>> Interpreter<I> {
    pub fn new(p: parser::Parser<I>) -> Interpreter<I> {
        Interpreter {
            vars: HashMap::new(),
            parser: p,
        }
    }

    pub fn parser(&self) -> &parser::Parser<I> { &self.parser }

    pub fn parser_mut(&mut self) -> &mut parser::Parser<I> { &mut self.parser }

    pub fn interpret(&mut self) -> Result<Value, Error> {
        self.parse_stmt()
    }

    pub fn expect_name(&mut self, ctx: Context) -> Result<String, Error> {
        match self.parser.token().kind() {
            TokenKind::Name => match self.parser.advance()? {
                Token::Name(s) => Ok(s),
                _ => unreachable!(),
            },
            x => Err(Error::Unexpected{ found: x, expected: TokenKind::Name, context: ctx }),
        }
    }

    pub fn expect_number(&mut self, ctx: Context) -> Result<usize, Error> {
        match self.parser.token().kind() {
            TokenKind::Number => match self.parser.advance()? {
                Token::Number(n) => Ok(n),
                _ => unreachable!(),
            },
            x => Err(Error::Unexpected{ found: x, expected: TokenKind::Number, context: ctx }),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Value, Error> {
        self.parser.parse_expr().map(Value::Rich).map_err(Into::into)
    }

    pub fn expect_rich_expr(&mut self, ctx: Context) -> Result<rich::Expr, Error> {
        let expr = self.parse_expr()?;

        let kind = expr.kind();
        if kind != ValueKind::Rich {
            Err(Error::BadValueKind{ found: kind, expected: ValueKind::Rich, context: ctx })
        } else {
            Ok(match expr {
                Value::Rich(x) => x,
                _ => unreachable!(),
            })
        }
    }

    pub fn parse_rewrite(&mut self) -> Result<Value, Error> {
        let mut expr = self.parse_expr()?;

        if let Token::Bracket(Side::Left) = self.parser.token() {
            loop {
                let name = self.expect_name(Context::Rewrite)?;
                let name = self.parser.namespace_mut().map(&name);

                match self.parser.token() {
                    &Token::Op(Op::Arrow) => {
                        self.parser.advance()?;
                        let repl = self.expect_rich_expr(Context::Rewrite)?;
                        expr = match expr {
                            Value::Rich(x) => Value::Rich(x.rewrite_into(name, repl)),
                            x => return Err(Error::BadValueKind{ found: x.kind(), expected: ValueKind::Rich, context: Context::Rewrite }),
                        };
                    },
                    &Token::Op(Op::Equal) => {
                        self.parser.advance()?;
                        let val = self.expect_number(Context::Rewrite)?;
                        expr = match expr {
                            Value::Rich(x) => x.into_simple()?.simplify_binding(name, val != 0),
                            Value::Simple(x) => x.simplify_binding(name, val != 0),
                            Value::Bool(b) => simple::EvalResult::Const(b),
                            Value::NormalForm(n) => n.into_simple()?.simplify_binding(name, val != 0),
                        }.into();
                    },
                    t => return Err(Error::Unexpected{ found: t.kind(), expected: TokenKind::Op, context: Context::Rewrite }),
                };

                if let &Token::Bracket(Side::Right) = self.parser.token() {
                    self.parser.advance()?;
                    return Ok(expr);
                }

                if let &Token::Op(Op::Comma) = self.parser.token() {
                    ()
                } else {
                    return Err(Error::Unexpected{ found: self.parser.token().kind(), expected: TokenKind::Op, context: Context::Rewrite })
                };
            }
        }

        Ok(expr)
    }

    pub fn parse_stmt(&mut self) -> Result<Value, Error> {
        let mut val = self.parse_rewrite()?;

        if let Token::Op(Op::Termin) = self.parser.token() {
            self.parser.advance()?;
            Ok(val)
        } else {
            Err(Error::Unexpected{ found: self.parser.token().kind(), expected: TokenKind::Op, context: Context::Statement })
        }
    }
}
