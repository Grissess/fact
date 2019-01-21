use crate::Kinded;

use std::num::ParseIntError;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum TokenKind {
    Name,
    Number,
    Op,
    Paren,
    Bracket,
    Brace,
    Eof,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Op {
    Negate,
    And,
    Or,
    Arrow,
    Equal,
    Define,
    Reference,
    Comma,
    Termin,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Side {
    Left,
    Right,
}

#[derive(Debug,Clone)]
pub enum Token {
    Name(String),
    Number(usize),
    Op(Op),
    Paren(Side),
    Bracket(Side),
    Brace(Side),
    Eof,
}

impl Kinded for Token {
    type Kind = TokenKind;

    fn kind(&self) -> TokenKind {
        match self {
            &Token::Name(_) => TokenKind::Name,
            &Token::Number(_) => TokenKind::Number,
            &Token::Op(_) => TokenKind::Op,
            &Token::Paren(_) => TokenKind::Paren,
            &Token::Bracket(_) => TokenKind::Bracket,
            &Token::Brace(_) => TokenKind::Brace,
            &Token::Eof => TokenKind::Eof,
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ErrorKind {
    UnexpectedEof,
    TooManyPushBacks,
    UnknownChar,
    ParseIntError,
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Context {
    AfterDash,
    AfterColon,
}

#[derive(Debug,Clone)]
pub enum Error {
    UnexpectedEof(Context),
    TooManyPushBacks{ incoming: char, present: char },
    UnknownChar(char),
    ParseIntError(ParseIntError),
}

impl Kinded for Error {
    type Kind = ErrorKind;

    fn kind(&self) -> ErrorKind {
        match self {
            &Error::UnexpectedEof(_) => ErrorKind::UnexpectedEof,
            &Error::TooManyPushBacks{..} => ErrorKind::TooManyPushBacks,
            &Error::UnknownChar(_) => ErrorKind::UnknownChar,
            &Error::ParseIntError(_) => ErrorKind::ParseIntError,
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<I: Iterator<Item=char>> {
    iter: I,
    pushback: Option<char>,
}

fn is_id_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_id_continue(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

impl<I: Iterator<Item=char>> Tokenizer<I> {
    pub fn new(iter: I) -> Tokenizer<I> {
        Tokenizer {
            iter: iter,
            pushback: None,
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        match self.pushback {
            Some(c) => {
                self.pushback = None;
                Some(c)
            },
            None => self.iter.next(),
        }
    }

    pub fn push(&mut self, c: char) -> Result<(), Error> {
        match self.pushback {
            Some(p) => Err(Error::TooManyPushBacks{ incoming: c, present: p }),
            None => {
                self.pushback = Some(c);
                Ok(())
            }
        }
    }

    pub fn skip_ws(&mut self) -> Result<(), Error> {
        loop {
            match self.next_char() {
                Some(c) => {
                    if !c.is_whitespace() {
                        self.push(c)?;
                        return Ok(());
                    }
                },
                None => return Ok(()),
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        self.skip_ws()?;
        let c = match self.next_char() {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };
        
        match c {
            '!' => Ok(Token::Op(Op::Negate)),
            '+' | '|' => Ok(Token::Op(Op::Or)),
            '*' | '^' => Ok(Token::Op(Op::And)),
            '=' => Ok(Token::Op(Op::Equal)),
            '-' => match self.next_char() {
                None => Err(Error::UnexpectedEof(Context::AfterDash)),
                Some(nc) => match nc {
                    '>' => Ok(Token::Op(Op::Arrow)),
                    nc => {
                        self.push(nc)?;
                        Ok(Token::Op(Op::Negate))
                    },
                },
            },
            ':' => match self.next_char() {
                None => Err(Error::UnexpectedEof(Context::AfterColon)),
                Some(nc) => match nc {
                    '=' => Ok(Token::Op(Op::Define)),
                    nc => Err(Error::UnknownChar(nc)),
                },
            },
            '@' => Ok(Token::Op(Op::Reference)),
            ',' => Ok(Token::Op(Op::Comma)),
            ';' => Ok(Token::Op(Op::Termin)),
            '(' => Ok(Token::Paren(Side::Left)),
            ')' => Ok(Token::Paren(Side::Right)),
            '[' => Ok(Token::Bracket(Side::Left)),
            ']' => Ok(Token::Bracket(Side::Right)),
            '{' => Ok(Token::Brace(Side::Left)),
            '}' => Ok(Token::Brace(Side::Right)),
            c if c.is_digit(10) => {
                let mut s = c.to_string();
                loop {
                    match self.next_char() {
                        None => break,
                        Some(nc) => {
                            if nc.is_digit(10) {
                                s.push(nc);
                            } else {
                                self.push(nc)?;
                                break;
                            }
                        },
                    }
                }
                println!("parsing as int: {:?}", s);
                Ok(Token::Number(s.parse().map_err(Error::ParseIntError)?))
            },
            c if is_id_start(c) => {
                let mut s = c.to_string();
                loop {
                    match self.next_char() {
                        None => break,
                        Some(nc) => {
                            if is_id_continue(nc) {
                                s.push(nc);
                            } else {
                                self.push(nc)?;
                                break;
                            }
                        },
                    }
                }
                Ok(Token::Name(s))
            },
            c => Err(Error::UnknownChar(c)),
        }
    }
}
