extern crate fact;

use fact::lang;
use fact::expr::{rich,simple};
use fact::expr::parse::{parser,tokenizer};
use fact::io::{BytesToChars,Unwrapped};

use std::io;
use std::io::Read;

fn main() {
    let mut stdin = io::stdin();
    let mut interp = lang::Interpreter::new(
        parser::Parser::new(
            tokenizer::Tokenizer::new(
                Unwrapped::new(
                    BytesToChars::new(stdin.lock().bytes())
                )
            )
        ).expect("failed to create parser")
    );

    loop {
        match interp.interpret() {
            Ok(v) => println!("{}", lang::Pretty(&v, interp.parser().namespace())),
            Err(e) => match e {
                lang::Error::ParserError(parser::Error::UnexpectedEof(c)) => {
                    println!("encountered eof in {:?}", c);
                    break
                },
                lang::Error::ParserError(parser::Error::Empty(c)) => {
                    println!("encountered empty production in {:?}", c);
                    interp.parser_mut().advance();
                    //break
                },
                e => {
                    interp.parser_mut().advance();
                    println!("error: {:?}", e);
                },
            },
        }
    }
}

fn bench_main() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("failed to get line");
    let mut parser = parser::Parser::new(tokenizer::Tokenizer::new(line.chars())).expect("failed to create parser");
    let expr: rich::Expr = parser.parse_expr().expect("failed to parse");
    // println!("{:?}", expr);
    // println!("{}", rich::Pretty::Expr(&expr, parser.namespace()));
    let expr = expr.into_simple().expect("failed to simplify");
    // println!("{:?}", expr);
    println!("{}", simple::Pretty::Expr(&expr, parser.namespace()));
}
