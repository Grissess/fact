use crate::Kinded;

#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Atom {
    Name(usize),
    GenSym(usize),
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum QuantifierKind {
    Exists,
    ForAll,
}

impl QuantifierKind {
    pub fn dual(self) -> QuantifierKind {
        match self {
            QuantifierKind::Exists => QuantifierKind::ForAll,
            QuantifierKind::ForAll => QuantifierKind::Exists,
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum BinOpKind {
    And,
    Or,
}

impl BinOpKind {
    pub fn dual(self) -> BinOpKind {
        match self {
            BinOpKind::And => BinOpKind::Or,
            BinOpKind::Or => BinOpKind::And,
        }
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum Expr {
    Atom(Atom),
    Function(Atom, Vec<Expr>),
    Quantify(QuantifierKind, Atom, Box<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum ExprKind {
    Atom,
    Function,
    Quantify,
    BinOp,
    Not,
}

impl Kinded for Expr {
    type Kind = ExprKind;

    fn kind(&self) -> ExprKind {
        match self {
            &Expr::Atom(_) => ExprKind::Atom,
            &Expr::Function(_, _) => ExprKind::Function,
            &Expr::Quantify(_, _, _) => ExprKind::Quantify,
            &Expr::BinOp(_, _, _) => ExprKind::BinOp,
            &Expr::Not(_) => ExprKind::Not,
        }
    }
}

#[derive(Debug)]
pub struct GenSymNamespace(usize);

impl GenSymNamespace {
    pub fn next(&mut self) -> Atom {
        let x = self.0;
        self.0 += 1;
        Atom::GenSym(x)
    }
}

impl Expr {
    pub fn is_valuant(&self) -> bool {
        match self {
            &Expr::Atom(_) | &Expr::Function(_, _) => true,
            _ => false,
        }
    }

    pub fn into_simple(self) -> Expr {
        match self {
            Expr::Not(be) => match *be {
                x@Expr::Atom(_) | x@Expr::Function(_, _) => Expr::Not(Box::new(x)),
                Expr::Quantify(qk, var, x) =>
                    Expr::Quantify(qk.dual(), var, Box::new(Expr::Not(x).into_simple())),
                Expr::BinOp(bk, lx, rx) => Expr::BinOp(
                    bk.dual(),
                    Box::new(Expr::Not(lx).into_simple()),
                    Box::new(Expr::Not(rx).into_simple())
                ),
                Expr::Not(x) => *x,
            },
            x => x,
        }
    }

    pub fn rewrite(self, at: Atom, rx: &Expr) -> Expr {
        match self {
            Expr::Atom(a) => if a == at {
                rx.clone()
            } else {
                Expr::Atom(a)
            },
            Expr::Function(

    pub fn into_skolem(self, ns: &mut GenSymNamespace) -> Expr {
        fn recur(ex: Expr, quants: mut Vec<&Atom>, ns: &mut GenSymNamespace) -> Expr {
            match ex {
                Expr::Quantify(qk, var, bx) => match qk {
                    QuantifierKind::ForAll => {
                        quants.push(&var);
                        let nx = recur(*bx, quants, ns);
                        quants.pop();
                        nx
                    },
                    QuantifierKind::Exists =>
                        Expr::Function(
                            ns.next(),
                            quants.iter().cloned().collect(),
}
