#![allow(warnings)]

// Goal #1: Eliminate the borrow check error in the `remove` method.

pub struct Map<K: Eq, V> {
    elements: Vec<(K, V)>,
}

impl<K: Eq, V> Map<K, V> {
    pub fn new() -> Self {
        Map { elements: vec![] }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.elements.push((key, value));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.elements.iter().rev().find(|pair| pair.0 == *key).map(|pair| &pair.1)
    }

    pub fn remove(&mut self, key: &K) {
				let mut i : Option<usize> = None;
        for (index, pair) in self.elements.iter().enumerate() {
            if pair.0 == *key {
							  i = Some(index);
                break;
            }
        }

        match i {
            Some(index) => {self.elements.remove(index);},
						None        => {},
        }
    }
}
