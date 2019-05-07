use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug,Clone)]
pub struct Namespace {
    map: HashMap<String, usize>,
    rev: Vec<Option<String>>,
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace {
            map: HashMap::new(),
            rev: Vec::new(),
        }
    }

    pub fn next(&self) -> usize { self.rev.len() }

    pub fn map<'s, 'k, K>(&'s mut self, k: &'k K) -> usize where String: From<&'k K>, K: ?Sized {
        let s = String::from(k);
        let n = self.next();
        match self.map.entry(s) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                self.rev.push(Some(String::from(k)));
                v.insert(n);
                n
            }
        }
    }

    pub fn alloc(&mut self) -> usize {
        let s = self.next();
        self.rev.push(None);
        s
    }

    pub fn unmap(&self, idx: usize) -> Option<&str> {
        self.rev.get(idx).unwrap_or(&None).as_ref().map(AsRef::as_ref)
    }
}
