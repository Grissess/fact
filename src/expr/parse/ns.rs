use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug,Clone)]
pub struct Namespace {
    map: HashMap<String, usize>,
    rev: Vec<Option<String>>,
    next: usize,
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace {
            map: HashMap::new(),
            rev: Vec::new(),
            next: 0usize,
        }
    }

    pub fn map<'s, 'k, K>(&'s mut self, k: &'k K) -> usize where String: From<&'k K>, K: ?Sized {
        let s = String::from(k);
        match self.map.entry(s) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let n = self.next;
                self.next += 1;
                self.rev.push(Some(String::from(k)));
                v.insert(n);
                n
            }
        }
    }

    pub fn alloc(&mut self) -> usize {
        let s = self.next;
        self.rev.push(None);
        self.next += 1;
        s
    }

    pub fn unmap(&self, idx: usize) -> Option<&str> {
        self.rev.get(idx).unwrap_or(&None).as_ref().map(AsRef::as_ref)
    }
}
