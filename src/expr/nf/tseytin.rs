use super::*;
use super::super::simple;
use super::super::parse::ns::Namespace;

pub fn tseytin(expr: &simple::Expr, ns: &mut Namespace) -> NormalForm {
    let mut clauses = NormalFormVec::new();

    fn recur(clauses: &mut NormalFormVec, expr: &simple::Expr, ns: &mut Namespace) -> simple::Atom {
        let kind = expr.kind();
        match expr {
            simple::Expr::Atom(a) => a.clone(),
            simple::Expr::And(a, b) | simple::Expr::Or(a, b) => {
                let ov = simple::Atom::Var(ns.alloc());
                let lv = recur(clauses, a, ns);
                let rv = recur(clauses, b, ns);

                clauses.extend_from_slice(&match kind {
                    simple::ExprKind::And => {
                        [
                            vec![ov.clone(), lv.clone().inverted(), rv.clone().inverted()],
                            vec![ov.clone().inverted(), lv],
                            vec![ov.clone().inverted(), rv],
                        ]
                    },
                    simple::ExprKind::Or => {
                        [
                            vec![ov.clone().inverted(), lv.clone(), rv.clone()],
                            vec![ov.clone(), lv.inverted()],
                            vec![ov.clone(), rv.inverted()],
                        ]
                    },
                    _ => unreachable!(),
                });

                ov
            },
        }
    };

    let ov = recur(&mut clauses, expr, ns);
    clauses.push(vec![ov]);
    NormalForm::Conjunctive(clauses)
}
