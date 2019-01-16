pub mod ns;
pub mod tokenizer;
pub mod parser;

use super::rich;

use std::str::FromStr;

impl FromStr for rich::Expr {
    type Err = parser::Error;

    fn from_str(s: &str) -> Result<rich::Expr, parser::Error> {
        parser::Parser::new(
            tokenizer::Tokenizer::new(s.chars())
        )?.parse_expr()
    }
}
