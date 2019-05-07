extern crate fact;

use fact::lang;
use fact::expr::{rich,simple};
use fact::expr::parse::{parser,tokenizer};
use fact::expr::nf::tseytin::tseytin;
use fact::expr::nf::CNF;
use fact::solver::cdcl::{ClauseSet, self};
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
            Ok(v) => {
                println!("{}", lang::Pretty(&v, interp.parser().namespace()));
                if let Ok(sv) = v.convert(lang::ValueKind::Simple, lang::Context::External) {
                    match sv {
                        lang::Value::Simple(simp) => {
                            println!("{}", simple::Pretty::Expr(&simp, interp.parser().namespace()));
                            if let Ok(cnf) = CNF::try_from(tseytin(&simp, interp.parser_mut().namespace_mut())) {
                                println!("{:?}", cnf);
                                let cset = ClauseSet::from(&cnf);
                                match cset.solve() {
                                    Some(ta) => println!("sat: {}", cdcl::Pretty(&ta, interp.parser().namespace())),
                                    None => println!("unsat"),
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                }
            },
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
