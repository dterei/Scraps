#![allow(warnings)]

// **Exercise 1.** Modify the signature of `get` to use an explicit
// lifetime name in the return type.
//
// **Exercise 2.** Change the signature of `get` to:
//
//     fn get<'a>(&'a mut self, key: &'a K) -> Option<&'a V>
//
// - Which test fails to compile?
// - Can you explain why?
//
// **Exercise 3.** Rewrite `insert` to not just push.

pub struct Map<K: Eq, V> {
    elements: Vec<(K, V)>,
}

impl<K: Eq, V> Map<K, V> {
    pub fn new() -> Self {
        Map { elements: vec![] }
    }

    pub fn insert(&mut self, key: K, value: V) {
        for pair in &mut self.elements {
            if pair.0 == key {
                pair.1 = value;
                return;
            }
        }
        self.elements.push((key, value));
    }

    pub fn get<'a>(&'a mut self, key: &'a K) -> Option<&'a V> {
        self.elements.iter().rev().find(|pair| pair.0 == *key).map(|pair| &pair.1)
    }
}

#[test]
fn basic() {
    let mut map = Map::new();

    map.insert('a', format!("alpha"));
    map.insert('b', format!("beta"));
    map.insert('g', format!("gamma"));

    assert_eq!(map.get(&'a'), Some(&format!("alpha")));
    assert_eq!(map.get(&'b'), Some(&format!("beta")));
    assert_eq!(map.get(&'g'), Some(&format!("gamma")));

    map.insert('a', format!("xxx"));

    assert_eq!(map.get(&'a'), Some(&format!("xxx")));
    assert_eq!(map.get(&'b'), Some(&format!("beta")));
    assert_eq!(map.get(&'g'), Some(&format!("gamma")));
}

#[test]
fn lock_receiver() {
    let mut map = Map::new();
    let mut string = format!("alpha");
    map.insert('a', string.clone());
    let a = 'a';
    let r = map.get(&a);
    assert_eq!(r, Some(&string));
}
