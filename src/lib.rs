pub mod expr;
pub mod lang;
pub mod io;

pub trait Kinded {
    type Kind;

    fn kind(&self) -> Self::Kind;
}
