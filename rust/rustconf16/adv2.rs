#![allow(warnings)]

// **Exercise 1.** For the method `get`, identify at least 4 lifetimes
// that must be inferred.
//
// **Exercise 2.** Modify the signature of `get` such that the method
// `get` fails to compile with a lifetime inference error.
//
// **Exercise 3.** Modify the signature of `get` such that the
// `do_not_compile` test fails to compile with a lifetime error
// (but `get` does not have any errors).
//
// **Exercise 4.** There are actually two ways to achieve Exercise 3.
// Can you find the other one?

pub struct Map<K: Eq, V> {
    elements: Vec<(K, V)>,
}

impl<K: Eq, V> Map<K, V> {
    pub fn new() -> Self {
        Map { elements: vec![] }
    }

    // lifetime 1: self   - NO: we use defaulting
    // lifetime 2: key    - NO: we use defaulting
    // lifetime 3: return - NO: we use defaulting
    // lifetime 4: matching_pair
    // lifetime 5: pair (1)
    // lifetime 6: pair (2)
    // lifetime 7: self.elements

    /* // ex1: default */
    /* pub fn get(&self, key: &K) -> Option<& V> */

    /* // ex2: bad lifetimes (1) */
    /* pub fn get<'a, 'b>(&'a self, key: &'a K) -> Option<&'b V> */
    /*     /* where 'a: 'b // good! */ */
    /*     where 'b: 'a // bad! */

    /* // ex2: bad lifetimes (2) -- need to return a type with infinite */
    /* // lifetime, i.e., static really. */
    /* pub fn get<'a>(&self, key: &K) -> Option<&'a V> */

    /* // ex3: make `do_not_compile` fail */
    /* pub fn get<'a>(&'a self, key: &'a K) -> Option<&'a V> */

    /* // ex4: make `do_not_compile` fail (2) */
    /* pub fn get<'a, 'b>(&'a self, key: &'b K) -> Option<&'a V> */
    /*     where 'b: 'a */

    pub fn get(&self, key: &K) -> Option<& V>
    {
        let matching_pair: Option<&(K, V)> = self.elements
            .iter()
            .rev()
            .find(|pair| pair.0 == *key);
        matching_pair.map(|pair| &pair.1)
    }
}

#[test]
fn do_not_compile() {
    let map: Map<char, String> = Map::new();
    let r;
    let key = &'c';
    r = map.get(key);
    panic!("If this test is running, your program compiled, and that's bad!");
    // why fail? because we don't just drop all local/frame variables at once,
    // we drop them in the reverse order that they were introduced. So if we
    // make `r` depend on `key`, then once we drop `key`, `r` isn't valid...
}
