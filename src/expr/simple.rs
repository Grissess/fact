use crate::Kinded;
use super::parse::ns::Namespace;

use std::fmt;
use std::collections::HashMap;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ExprKind {
    Atom,
    And,
    Or,
}

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
pub enum Atom {
    Var(usize),
    Neg(usize),
}

impl Atom {
    pub fn binding(name: usize, value: bool) -> Atom {
        if value {
            Atom::Var(name)
        } else {
            Atom::Neg(name)
        }
    }

    pub fn name(&self) -> usize {
        match self {
            &Atom::Var(x) => x,
            &Atom::Neg(x) => x,
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            &Atom::Var(_) => true,
            &Atom::Neg(_) => false,
        }
    }

    pub fn inverted(self) -> Atom {
        match self {
            Atom::Var(x) => Atom::Neg(x),
            Atom::Neg(x) => Atom::Var(x),
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Atom::Var(i) => write!(f, "<{}>", i),
            &Atom::Neg(i) => write!(f, "!<{}>", i),
        }
    }
}

#[derive(Debug,Clone)]
pub enum Expr {
    Atom(Atom),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Error {
    BadKind,
    Empty,
}

#[derive(Debug,Clone)]
pub enum EvalResult {
    Expr(Expr),
    Const(bool),
}

impl EvalResult {
    pub fn simplify(k: ExprKind, el: EvalResult, er: EvalResult) -> EvalResult {
        match (k, el, er) {
            (ExprKind::And, EvalResult::Const(false), _) => EvalResult::Const(false),
            (ExprKind::And, _, EvalResult::Const(false)) => EvalResult::Const(false),
            (ExprKind::And, EvalResult::Const(true), r) => r,
            (ExprKind::And, l, EvalResult::Const(true)) => l,
            (ExprKind::And, EvalResult::Expr(l), EvalResult::Expr(r)) => EvalResult::Expr(Expr::And(Box::new(l), Box::new(r))),
            (ExprKind::Or, EvalResult::Const(true), _) => EvalResult::Const(true),
            (ExprKind::Or, _, EvalResult::Const(true)) => EvalResult::Const(true),
            (ExprKind::Or, EvalResult::Const(false), r) => r,
            (ExprKind::Or, l, EvalResult::Const(false)) => l,
            (ExprKind::Or, EvalResult::Expr(l), EvalResult::Expr(r)) => EvalResult::Expr(Expr::Or(Box::new(l), Box::new(r))),
            _ => unreachable!(),
        }
    }
}

pub struct AtomCount(HashMap<usize, usize>);

impl AtomCount {
    pub fn num_vars(&self) -> usize {
        self.0.len()
    }

    pub fn vars(&self) -> Vec<usize> {
        self.0.keys().map(|&x| x).collect()
    }

    pub fn cardinality(&self, var: usize) -> usize {
        self.0.get(&var).map(|&x| x).unwrap_or(0)
    }
}

impl Expr {
    pub fn from_op_exs(kind: ExprKind, exprs: &[Expr]) -> Result<Expr, Error> {
        if exprs.len() == 0 { return Err(Error::Empty); }

        let cons = match kind {
            ExprKind::And => Expr::And,
            ExprKind::Or => Expr::Or,
            _ => return Err(Error::BadKind),
        };
        
        fn recur<F: Copy + Fn(Box<Expr>, Box<Expr>) -> Expr>(cn: F, part: &[Expr]) -> Expr {
            let len = part.len();
            match len {
                1 => part[0].clone(),
                2 => cn(Box::new(part[0].clone()), Box::new(part[1].clone())),
                x if x > 2 => {
                    let mid = x / 2;
                    cn(Box::new(recur(cn, &part[..mid])), Box::new(recur(cn, &part[mid..])))
                },
                _ => unreachable!(),
            }
        }

        Ok(recur(cons, exprs))
    }

    pub fn simplify_binding(self, var: usize, val: bool) -> EvalResult {
        match self {
            Expr::Atom(a) => {
                if a.name() == var {
                    EvalResult::Const(if a.is_positive() { val } else { !val })
                } else {
                    EvalResult::Expr(Expr::Atom(a))
                }
            },
            n @ Expr::And(_, _) | n @ Expr::Or(_, _) => {
                let (k, l, r) = match n {
                    Expr::And(l, r) => (ExprKind::And, l, r),
                    Expr::Or(l, r) => (ExprKind::Or, l, r),
                    _ => unreachable!(),
                };
                let (el, er) = (l.simplify_binding(var, val), r.simplify_binding(var, val));
                EvalResult::simplify(k, el, er)
            }
        }
    }

    pub fn simplify_tauto(self) -> EvalResult {
        match self {
            Expr::Atom(a) => EvalResult::Expr(Expr::Atom(a)),
            n @ Expr::And(_, _) | n @ Expr::Or(_, _) => {
                let (k, l, r) = match n {
                    Expr::And(l, r) => (ExprKind::And, l, r),
                    Expr::Or(l, r) => (ExprKind::Or, l, r),
                    _ => unreachable!(),
                };
                let (el, er) = (l.simplify_tauto(), r.simplify_tauto());
                match EvalResult::simplify(k, el, er) {
                    EvalResult::Const(b) => EvalResult::Const(b),
                    EvalResult::Expr(x) => match x {
                        Expr::And(l, r) => match (*l, *r) {
                            (Expr::Atom(l), Expr::Atom(r)) => match (l, r) {
                                (Atom::Var(i), Atom::Var(j)) if i == j => EvalResult::Expr(Expr::Atom(Atom::Var(i))),
                                (Atom::Neg(i), Atom::Neg(j)) if i == j => EvalResult::Expr(Expr::Atom(Atom::Neg(i))),
                                (Atom::Var(i), Atom::Neg(j)) if i == j => EvalResult::Const(false),
                                (Atom::Neg(i), Atom::Var(j)) if i == j => EvalResult::Const(false),
                                (l, r) => EvalResult::Expr(Expr::And(Box::new(Expr::Atom(l)), Box::new(Expr::Atom(r)))),
                            },
                            (l, r) => EvalResult::Expr(Expr::And(Box::new(l), Box::new(r))),
                        },
                        Expr::Or(l, r) => match (*l, *r) {
                            (Expr::Atom(l), Expr::Atom(r)) => match (l, r) {
                                (Atom::Var(i), Atom::Var(j)) if i == j => EvalResult::Expr(Expr::Atom(Atom::Var(i))),
                                (Atom::Neg(i), Atom::Neg(j)) if i == j => EvalResult::Expr(Expr::Atom(Atom::Neg(i))),
                                (Atom::Var(i), Atom::Neg(j)) if i == j => EvalResult::Const(true),
                                (Atom::Neg(i), Atom::Var(j)) if i == j => EvalResult::Const(true),
                                (l, r) => EvalResult::Expr(Expr::Or(Box::new(Expr::Atom(l)), Box::new(Expr::Atom(r)))),
                            },
                            (l, r) => EvalResult::Expr(Expr::Or(Box::new(l), Box::new(r))),
                        },
                        _ => unreachable!(),
                    },
                }
            },
        }
    }

    pub fn atom_count(&self) -> AtomCount {
        let mut count: HashMap<usize, usize> = HashMap::new();

        fn recur(expr: &Expr, count: &mut HashMap<usize, usize>) {
            match expr {
                Expr::Atom(a) => *count.entry(a.name()).or_insert(0) += 1,
                Expr::And(l, r) | Expr::Or(l, r) => {
                    recur(l, count);
                    recur(r, count);
                },
            }
        }

        recur(&self, &mut count);

        AtomCount(count)
    }
}

impl Kinded for Expr {
    type Kind = ExprKind;

    fn kind(&self) -> ExprKind {
        match self {
            &Expr::Atom(_) => ExprKind::Atom,
            &Expr::And(_, _) => ExprKind::And,
            &Expr::Or(_, _) => ExprKind::Or,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(a) => write!(f, "{}", a),
            Expr::And(a, b) => write!(f, "({} * {})", a, b),
            Expr::Or(a, b) => write!(f, "({} + {})", a, b),
        }
    }
}

#[derive(Debug)]
pub enum Pretty<'a, 'b> {
    Expr(&'a Expr, &'b Namespace),
    Atom(&'a Atom, &'b Namespace),
}

impl<'a, 'b> fmt::Display for Pretty<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pretty::Expr(expr, ns) => {
                match expr {
                    Expr::Atom(a) => write!(f, "{}", Pretty::Atom(a, ns)),
                    Expr::And(a, b) => write!(f, "({} * {})", Pretty::Expr(a, ns), Pretty::Expr(b, ns)),
                    Expr::Or(a, b) => write!(f, "({} + {})", Pretty::Expr(a, ns), Pretty::Expr(b, ns)),
                }
            },
            Pretty::Atom(atom, ns) => {
                match atom {
                    Atom::Var(i) => write!(f, "{}", match ns.unmap(*i) {
                        Some(s) => s.to_string(),
                        None => format!("{}", Atom::Var(*i)),
                    }),
                    Atom::Neg(i) => write!(f, "!{}", match ns.unmap(*i) {
                        Some(s) => s.to_string(),
                        None => format!("{}", Atom::Neg(*i)),
                    }),
                }
            },
        }
    }
}
