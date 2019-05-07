use super::expr::simple::Atom;
use super::expr::nf::CNF;
use std::collections::{hash_map, HashMap};

pub mod cdcl;

pub trait TruthAssignment {
    fn atoms(&self) -> Vec<Atom>;
    fn len(&self) -> usize;
    fn value(&self, name: usize) -> Option<bool>;
}

#[derive(Debug,Clone)]
pub struct HashMapTruthAssignment(HashMap<usize, bool>);

impl HashMapTruthAssignment {
    pub fn new() -> HashMapTruthAssignment {
        HashMapTruthAssignment(HashMap::new())
    }

    pub fn set_name(&mut self, name: usize, value: bool) {
        self.0.insert(name, value);
    }

    pub fn has_name(&self, name: usize) -> bool {
        self.0.contains_key(&name)
    }

    pub fn get_name(&self, name: usize) -> Option<bool> {
        self.0.get(&name).map(|&x| x)
    }

    pub fn get_atom(&self, atom: &Atom) -> Option<bool> {
        self.get_name(atom.name()).map(|b| if atom.is_positive() { b } else { !b })
    }
}

impl TruthAssignment for HashMapTruthAssignment {
    fn atoms(&self) -> Vec<Atom> {
        self.0.iter().map(|(&nm, &v)| Atom::binding(nm, v)).collect()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn value(&self, name: usize) -> Option<bool> {
        self.get_name(name)
    }
}

pub trait BranchPolicy {
    fn new(cnf: &CNF) -> Self;
    fn select<T: TruthAssignment>(&mut self, assignment: &T) -> usize;
}
