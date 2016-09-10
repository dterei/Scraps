#![allow(warnings)]

// **Exercise 1.** Consider a method `fn remove(self) -> V` that
// removes a value from an entry:
// - Which of the `Entry` types does this method belong on?
// - Implement the method.
//
// **Exercise 2.** The entry types represent a limited sort of state
// machine that does not allow very many transitions. You could also
// permit going back and forth between the empty and full states.
// Modify the methods `insert` and `remove` to have the following
// type signatures:
//
// - `fn insert(self) -> FoundEntry<'map, K, V>`
// - `fn remove(self) -> (NotFoundEntry<'map, K, V>, V)`
//
// You can uncomment the test `exercise_2` to see how they should be
// used.

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

    pub fn get(&self, key: &K) -> Option<&V> {
        self.elements.iter().rev().find(|pair| pair.0 == *key).map(|pair| &pair.1)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.elements.iter_mut().rev().find(|pair| pair.0 == *key).map(|pair| &mut pair.1)
    }

    fn index_of(&self, key: &K) -> Option<usize> {
        self.elements.iter().position(|pair| pair.0 == *key)
    }

    pub fn entry<'map>(&'map mut self, key: K) -> Entry<'map, K, V> {
        match self.index_of(&key) {
            Some(position) => {
                Entry::Found(FoundEntry {
                    index: position,
                    elements: &mut self.elements,
                })
            }
            None => {
                Entry::NotFound(NotFoundEntry {
                    key: key,
                    elements: &mut self.elements,
                })
            }
        }
    }
}

pub enum Entry<'map, K, V>
    where K: Eq,
          K: 'map,
          V: 'map
{
    Found(FoundEntry<'map, K, V>),
    NotFound(NotFoundEntry<'map, K, V>),
}

pub struct FoundEntry<'map, K, V>
    where K: Eq,
          K: 'map,
          V: 'map
{
    index: usize,
    elements: &'map mut Vec<(K, V)>,
}

pub struct NotFoundEntry<'map, K, V>
    where K: Eq,
          K: 'map,
          V: 'map
{
    key: K,
    elements: &'map mut Vec<(K, V)>,
}

impl<'map, K: Eq, V> Entry<'map, K, V> {
    fn or_insert(self, value: V) -> &'map mut V {
        match self {
            Entry::Found(mut data) => &mut data.elements[data.index].1,
            Entry::NotFound(mut data) => {
                data.elements.push((data.key, value));
                &mut data.elements.last_mut().unwrap().1
            }
        }
    }
}

impl<'map, K: Eq, V> FoundEntry<'map, K, V> {
    fn get(self) -> &'map mut V {
        &mut self.elements[self.index].1
    }

    fn remove(self) -> V {
        unimplemented!()
    }
}

impl<'map, K: Eq, V> NotFoundEntry<'map, K, V> {
    fn insert(self, value: V) -> &'map mut V {
        self.elements.push((self.key, value));
        &mut self.elements.last_mut().unwrap().1
    }
}

#[test]
fn or_insert() {
    let mut map = Map::new();
    map.insert('a', format!("alpha"));
    assert_eq!(map.entry('a').or_insert(format!("beta")), &format!("alpha"));
}

#[test]
fn exercise_1() {
    let mut map = Map::new();
    map.insert('a', format!("alpha"));
    let data = match map.entry('a') {
        Entry::Found(data) => data.remove(),
        Entry::NotFound(_) => panic!(),
    };
    assert_eq!(data, format!("alpha"));
}

//#[test]
//fn exercise_2() {
//    let mut map = Map::new();
//    map.insert('a', format!("alpha"));
//
//    {
//        let not_found = match map.entry('a') {
//            Entry::NotFound(data) => data,
//            Entry::Found(data) => data.remove().0,
//        };
//        not_found.insert(format!("beta"));
//    }
//
//    assert_eq!(map.get(&'a'), Some(&format!("beta")));
//}
