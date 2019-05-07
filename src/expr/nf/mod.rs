use super::simple;
use crate::Kinded;

pub mod distr;
pub mod tseytin;

#[derive(Debug,Clone)]
pub struct NormalFormVec(pub Vec<Vec<simple::Atom>>);

impl NormalFormVec {
    pub fn new() -> NormalFormVec {
        NormalFormVec(Vec::new())
    }

    pub fn push(&mut self, v: Vec<simple::Atom>) {
        self.0.push(v);
    }

    pub fn extend<I: IntoIterator<Item=Vec<simple::Atom>>>(&mut self, iter: I) {
        self.0.extend(iter);
    }

    pub fn extend_from_slice(&mut self, slice: &[Vec<simple::Atom>]) {
        self.0.extend_from_slice(slice);
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum NormalFormKind {
    Conjunctive,
    Disjunctive,
}

#[derive(Debug,Clone)]
pub enum NormalForm {
    Conjunctive(NormalFormVec),
    Disjunctive(NormalFormVec),
}

impl Kinded for NormalForm {
    type Kind = NormalFormKind;

    fn kind(&self) -> NormalFormKind {
        match self {
            &NormalForm::Conjunctive(_) => NormalFormKind::Conjunctive,
            &NormalForm::Disjunctive(_) => NormalFormKind::Disjunctive,
        }
    }
}

impl NormalForm {
    pub fn into_simple(self) -> Result<simple::Expr, simple::Error> {
        let (inner, outer, v) = match self {
            NormalForm::Conjunctive(v) => (simple::ExprKind::Or, simple::ExprKind::And, v),
            NormalForm::Disjunctive(v) => (simple::ExprKind::And, simple::ExprKind::Or, v),
        };

        let len = v.0.len();
        simple::Expr::from_op_exs(
            outer,
            v.0.into_iter().map(|x| simple::Expr::from_op_exs(inner, x.into_iter().map(simple::Expr::Atom).collect::<Vec<_>>().as_slice())).try_fold(
                Vec::with_capacity(len),
                |mut v, r| { v.push(r?); Ok(v) },
            )?.as_slice(),
        )
    }
}

#[derive(Debug,Clone)]
pub enum Error {
    BadKind(NormalFormKind),
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ErrorKind {
    BadKind,
}

impl Kinded for Error {
    type Kind = ErrorKind;

    fn kind(&self) -> ErrorKind {
        match self {
            &Error::BadKind(_) => ErrorKind::BadKind,
        }
    }
}
 
#[derive(Debug,Clone)]
pub struct CNF(pub NormalFormVec);

impl CNF {
    pub fn try_from(nf: NormalForm) -> Result<CNF, (Error, NormalForm)> {
        match nf {
            NormalForm::Conjunctive(v) => Ok(CNF(v)),
            x => Err((Error::BadKind(x.kind()), x)),
        }
    }
}
