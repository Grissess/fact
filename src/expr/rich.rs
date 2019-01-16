use crate::Kinded;
use super::simple;
pub use super::simple::Atom;
use super::parse::ns::Namespace;

use std::fmt;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ExprKind {
    Atom,
    And,
    Or,
    Not,
    Implies,
    Equivalent,
}

#[derive(Debug,Clone)]
pub enum Expr {
    Atom(Atom),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
    Implies(Box<Expr>, Box<Expr>),
    Equivalent(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn de_morgan(self) -> Expr {
        if let Expr::Not(ex) = self {
            match *ex {
                Expr::Not(e) => *e,
                n @ Expr::And(_) | n @ Expr::Or(_) => {
                    (match n {
                        Expr::And(_) => Expr::Or,
                        Expr::Or(_) => Expr::And,
                        _ => unreachable!(),
                    })((match n {
                        Expr::And(v) => v,
                        Expr::Or(v) => v,
                        _ => unreachable!(),
                    }).iter().cloned().map(Box::new).map(Expr::Not).collect()).de_morgan()
                },
                Expr::Atom(a) => Expr::Atom(a.inverted()),
                e => Expr::Not(Box::new(e)),
            }
        } else {
            self
        }
    }

    pub fn elim_implication(self) -> Expr {
        match self {
            Expr::Implies(ant, con) => Expr::Or(vec![Expr::Not(ant), *con]).elim_implication(),
            Expr::Equivalent(a, b) => Expr::Or(vec![
                Expr::And(vec![(*a).clone(), (*b).clone()]).elim_implication(),
                Expr::And(vec![Expr::Not(a), Expr::Not(b)]).elim_implication(),
            ]),
            Expr::And(v) => Expr::And(v.into_iter().map(Expr::elim_implication).collect()),
            Expr::Or(v) => Expr::Or(v.into_iter().map(Expr::elim_implication).collect()),
            Expr::Not(e) => Expr::Not(Box::new(e.elim_implication())),
            x => x,
        }
    }

    pub fn into_simple(self) -> Result<simple::Expr, simple::Error> {
        match self.elim_implication().de_morgan() {
            Expr::Atom(a) => Ok(simple::Expr::Atom(a)),
            n @ Expr::And(_) | n @ Expr::Or(_) => {
                let (v, k) = match n {
                    Expr::And(v) => (v, simple::ExprKind::And),
                    Expr::Or(v) => (v, simple::ExprKind::Or),
                    _ => unreachable!(),
                };
                let len = v.len();
                simple::Expr::from_op_exs(k,
                    &v.into_iter().map(Expr::into_simple).try_fold(
                        Vec::with_capacity(len),
                        |mut v, r| {v.push(r?); Ok(v) },
                    )?[..]
                )
            },
            x => panic!("unreachable: found a {:?}", x),
        }
    }

    pub fn rewrite_into(self, var: usize, expr: Expr) -> Expr {
        let kind = self.kind();
        match self {
            Expr::Atom(a) => {
                if a.name() == var {
                    if a.is_positive() { expr } else { Expr::Not(Box::new(expr)) }
                } else {
                    Expr::Atom(a)
                }
            },
            Expr::And(v) | Expr::Or(v) => {
                let cons = match kind {
                    ExprKind::And => Expr::And,
                    ExprKind::Or => Expr::Or,
                    _ => unreachable!(),
                };
                cons(v.into_iter().map(|x| x.rewrite_into(var, expr.clone())).collect())
            },
            Expr::Not(x) => Expr::Not(Box::new(x.rewrite_into(var, expr))),
            Expr::Implies(a, b) => {
                Expr::Implies(
                    Box::new(a.rewrite_into(var, expr.clone())),
                    Box::new(b.rewrite_into(var, expr)),
                )
            },
            Expr::Equivalent(a, b) => {
                Expr::Equivalent(
                    Box::new(a.rewrite_into(var, expr.clone())),
                    Box::new(b.rewrite_into(var, expr)),
                )
            },
        }
    }
}

impl Kinded for Expr {
    type Kind = ExprKind;

    fn kind(&self) -> ExprKind {
        match self {
            &Expr::Atom(_) => ExprKind::Atom,
            &Expr::And(_) => ExprKind::And,
            &Expr::Or(_) => ExprKind::Or,
            &Expr::Not(_) => ExprKind::Not,
            &Expr::Implies(_, _) => ExprKind::Implies,
            &Expr::Equivalent(_, _) => ExprKind::Equivalent,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(a) => write!(f, "{}", a),
            Expr::And(v) | Expr::Or(v) => {
                write!(f, "({})", v.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join(match self.kind() {
                    ExprKind::And => " * ",
                    ExprKind::Or => " + ",
                    _ => unreachable!(),
                }))
            },
            Expr::Not(a) => write!(f, "!{}", a),
            Expr::Implies(a, b) => write!(f, "({} -> {})", a, b),
            Expr::Equivalent(a, b) => write!(f, "({} = {})", a, b),
        }
    }
}

pub enum Pretty<'a, 'b> {
    Expr(&'a Expr, &'b Namespace),
}

impl<'a, 'b> fmt::Display for Pretty<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pretty::Expr(expr, ns) => {
                match expr {
                    Expr::Atom(a) => write!(f, "{}", simple::Pretty::Atom(a, ns)),
                    Expr::And(v) | Expr::Or(v) => {
                        write!(f, "({})", v.iter().map(|x| format!("{}", Pretty::Expr(x, ns))).collect::<Vec<_>>().join(match expr.kind() {
                            ExprKind::And => " * ",
                            ExprKind::Or => " + ",
                            _ => unreachable!(),
                        }))
                    },
                    Expr::Not(a) => write!(f, "!{}", Pretty::Expr(a, ns)),
                    Expr::Implies(a, b) => write!(f, "({} -> {})", Pretty::Expr(a, ns), Pretty::Expr(b, ns)),
                    Expr::Equivalent(a, b) => write!(f, "({} = {})", Pretty::Expr(a, ns), Pretty::Expr(b, ns)),
                }
            },
        }
    }
}
