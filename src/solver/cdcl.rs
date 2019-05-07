use crate::Kinded;
use crate::expr::nf::{NormalForm, NormalFormVec, NormalFormKind, CNF};
use crate::expr::simple::Atom;
use crate::expr::parse::ns::Namespace;
use super::{TruthAssignment, BranchPolicy, HashMapTruthAssignment};
use rpds::{HashTrieMap, HashTrieSet};

use std::fmt;
use std::collections::HashSet;

#[derive(Debug,Clone)]
pub enum CDCLResult {
    Sat{ assignment: HashTrieMap<usize, bool>, formula: CNF},
    Unsat{ formula: CNF },
    Backtrack{ participants: HashTrieSet<usize>, formula: CNF },
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum CDCLResultKind {
    Sat,
    Unsat,
    Backtrack,
}

impl Kinded for CDCLResult {
    type Kind = CDCLResultKind;

    fn kind(&self) -> CDCLResultKind {
        match self {
            &CDCLResult::Sat{..} => CDCLResultKind::Sat,
            &CDCLResult::Unsat{..} => CDCLResultKind::Unsat,
            &CDCLResult::Backtrack{..} => CDCLResultKind::Backtrack,
        }
    }
}

pub struct ImplicationGraph(HashTrieMap<Atom, HashTrieSet<Atom>>);

impl ImplicationGraph {
    pub fn with_edge(&self, a: Atom, b: Atom) -> ImplicationGraph {
        let mut newmap = if !self.0.contains_key(&a) {
            let mut newset = HashTrieSet::new();
            newset.insert_mut(b.clone());
            self.0.insert(a.clone(), newset)
        } else {
            let set = self.0.get(&a).unwrap().insert(b.clone());
            self.0.insert(a.clone(), set)
        };
        if !self.0.contains_key(&b) {
            let mut newset = HashTrieSet::new();
            newset.insert_mut(a);
            newmap.insert(b, newset);
        } else {
            let set = newmap.get(&b).unwrap().insert(a);
            newmap.insert_mut(b, set);
        }
        ImplicationGraph(newmap)
    }

    pub fn cut<I: Iterator<Item=Atom>>(&self, nodeiter: I) -> HashTrieSet<Atom> {
        let mut set = HashTrieSet::new();

        for node in nodeiter {
            if let Some(connected) = self.0.get(&node) {
                for ncon in connected {
                    set.insert_mut(ncon.clone());
                }
            }
        }

        set
    }
}

#[derive(Debug,Clone)]
pub struct HashTrieMapTruthAssignment(HashTrieMap<usize, bool>);

impl HashTrieMapTruthAssignment {
    pub fn new() -> HashTrieMapTruthAssignment {
        HashTrieMapTruthAssignment(HashTrieMap::new())
    }

    pub fn with_binding(&self, binding: &Atom) -> HashTrieMapTruthAssignment {
        HashTrieMapTruthAssignment(self.0.insert(binding.name(), binding.is_positive()))
    }

    pub fn set_binding(&mut self, binding: &Atom) {
        self.0.insert_mut(binding.name(), binding.is_positive())
    }

    pub fn has_name(&self, name: usize) -> bool {
        self.0.contains_key(&name)
    }

    pub fn atoms_set(&self) -> HashTrieSet<Atom> {
        let mut set = HashTrieSet::new();

        for (&nm, &v) in &self.0 {
            set.insert_mut(Atom::binding(nm, v));
        }

        set
    }
}

impl fmt::Display for HashTrieMapTruthAssignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0.iter().map(|(k, v)| format!("{}={}", k, if *v { 1 } else { 0 })).collect::<Vec<_>>().join(", "))
    }
}

#[derive(Debug)]
pub struct Pretty<'a, 'b>(pub &'a HashTrieMapTruthAssignment, pub &'b Namespace);

impl<'a, 'b> fmt::Display for Pretty<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (named, unnamed) = (self.0).0.iter().partition::<Vec<_>, _>(|(k, v)| self.1.unmap(**k).is_some());
        write!(f, "[{}]",
            named.iter().map(|(k, v)| format!("{}={}", self.1.unmap(**k).unwrap(), if **v { 1 } else { 0 }))
            .chain(unnamed.iter().map(|(k, v)| format!("{}={}", k, if **v { 1 } else { 0 })))
            .collect::<Vec<_>>().join(", ")
        )
    }
}

impl TruthAssignment for HashTrieMapTruthAssignment {
    fn atoms(&self) -> Vec<Atom> {
        self.0.iter().map(|(&nm, &v)| Atom::binding(nm, v)).collect()
    }

    fn len(&self) -> usize {
        self.0.size()
    }

    fn value(&self, name: usize) -> Option<bool> {
        self.0.get(&name).map(|&b| b)
    }
}

impl From<HashTrieMapTruthAssignment> for HashMapTruthAssignment {
    fn from(assignment: HashTrieMapTruthAssignment) -> HashMapTruthAssignment {
        HashMapTruthAssignment(assignment.0.iter().map(|(&nm, &v)| (nm, v)).collect())
    }
}

#[derive(Debug,Clone)]
pub struct ClauseSet(Vec<HashTrieSet<Atom>>);

impl<'a> From<&'a CNF> for ClauseSet {
    fn from(cnf: &'a CNF) -> ClauseSet {
        let mut set: Vec<HashTrieSet<Atom>> = Vec::new();

        for clause in &(cnf.0).0 {
            let mut cset: HashTrieSet<Atom> = HashTrieSet::new();
            
            for atom in clause {
                cset.insert_mut(atom.clone());
            }

            set.push(cset);
        }

        ClauseSet(set)
    }
}

impl ClauseSet {
    pub fn elim_tauto(self) -> ClauseSet {
        ClauseSet(self.0.into_iter().map(|mut clause| {
            let names = clause.iter().map(|x| x.name()).collect::<HashSet<_>>();

            for name in names {
                let pos = Atom::Var(name);
                let neg = Atom::Neg(name);

                if clause.contains(&pos) && clause.contains(&neg) {
                    clause.remove_mut(&pos);
                    clause.remove_mut(&neg);
                }
            }

            clause
        }).collect())
    }

    pub fn apply_binding(&self, binding: &Atom) -> ClauseSet {
        let inv_binding = binding.clone().inverted();

        ClauseSet(self.0.iter().filter_map(|clause|
            if clause.contains(&binding) {
                None
            } else {
                Some(clause.remove(&inv_binding))
            }
        ).collect())
    }

    pub fn is_unsat(&self) -> bool {
        self.0.iter().any(|x| x.is_empty())
    }

    pub fn is_sat(&self) -> bool {
        self.0.is_empty()
    }

    pub fn units(&self) -> Vec<Atom> {
        self.0.iter().filter_map(|clause|
            if clause.size() == 1 {
                Some(clause.iter().next().unwrap().clone())
            } else {
                None
            }
        ).collect()
    }

    pub fn unit_propagate(&self, mut ta: HashTrieMapTruthAssignment) -> (ClauseSet, HashTrieMapTruthAssignment) {
        let units = self.units();
        let mut cur = self.clone();

        for unit in &units {
            ta = ta.with_binding(unit);
            cur = cur.apply_binding(unit);
        }

        (cur, ta)
    }

    pub fn try_binding(&self, bvar: &Atom, ta: &HashTrieMapTruthAssignment) -> Option<HashTrieMapTruthAssignment> {
        let bound_ta = ta.with_binding(bvar);
        let bound_self = self.apply_binding(bvar);
        let (bound_self, bound_ta) = bound_self.unit_propagate(bound_ta);
        bound_self.solve_with(bound_ta)
    }

    pub fn solve_with(&self, ta: HashTrieMapTruthAssignment) -> Option<HashTrieMapTruthAssignment> {
        if self.is_sat() { return Some(ta); }
        if self.is_unsat() { return None; }

        // TODO: Better branching policy here
        let mut bvar = self.0.iter().find_map(|clause|
            if !clause.is_empty() {
                Some(clause.iter().next().unwrap().clone())
            } else {
                None
            }
        ).unwrap();

        if let Some(ta) = self.try_binding(&bvar, &ta) { return Some(ta); }

        bvar = bvar.inverted();

        if let Some(ta) = self.try_binding(&bvar, &ta) { return Some(ta); }

        None
    }

    pub fn solve(&self) -> Option<HashTrieMapTruthAssignment> {
        let (simpl_self, simpl_ta) = self.unit_propagate(HashTrieMapTruthAssignment::new());
        simpl_self.solve_with(simpl_ta)
    }
}
