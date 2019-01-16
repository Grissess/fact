use super::*;
use super::super::simple;

pub fn distribute(expr: &simple::Expr, kind: NormalFormKind) -> NormalForm {
    let mut clauses = NormalFormVec::new();

    struct Config<'a, F, G>
        where F: Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr + ?Sized,
              G: Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr + ?Sized
    {
        inner_cons: &'a F,
        outer_cons: &'a G,
        inner_kind: simple::ExprKind,
        outer_kind: simple::ExprKind,
    }

    let cfg = match kind {
        NormalFormKind::Conjunctive => Config {
            inner_cons: &simple::Expr::Or as &Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr,
            outer_cons: &simple::Expr::And as &Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr,
            inner_kind: simple::ExprKind::Or,
            outer_kind: simple::ExprKind::And,
        },
        NormalFormKind::Disjunctive => Config {
            inner_cons: &simple::Expr::And as &Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr,
            outer_cons: &simple::Expr::Or as &Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr,
            inner_kind: simple::ExprKind::And,
            outer_kind: simple::ExprKind::Or,
        },
    };

    fn recur<'a, F, G>(clauses: &mut NormalFormVec, expr: &simple::Expr, cfg: &Config<'a, F, G>)
        where F: Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr + ?Sized,
              G: Fn(Box<simple::Expr>, Box<simple::Expr>) -> simple::Expr + ?Sized
    {
        unimplemented!()
    }

    unimplemented!()
}
