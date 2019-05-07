extern crate rpds;

use std::borrow::Borrow;

pub mod expr;
pub mod lang;
pub mod io;
pub mod solver;
pub mod fol;

pub trait Kinded {
    type Kind;

    fn kind(&self) -> Self::Kind;
}
