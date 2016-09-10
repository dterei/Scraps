#![allow(dead_code)]

// use std::f32::INFINITY;
use std::sync::Arc;
use std::thread;
use std::sync::mpsc::Sender;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::channel;

type StoreResult = (String,f32);

struct Store {
    name: String,
    items: Vec<Item>,
}

#[derive(Debug)]
struct Item {
    name: &'static str,
    price: f32,
}

impl Store {
    fn new(name: String) -> Store {
        Store {
            name: name,
            items: vec![],
        }
    }

    fn add_item(&mut self, item: Item) {
        self.items.push(item);
    }

    fn price(&self, item_name: &str) -> f32 {
        for item in &self.items {
            if item.name == item_name {
                return item.price;
            }
        }

        panic!("no such item {:?}", item_name);
    }

    fn total_price(&self, shopping_list: &[&str]) -> f32 {
        shopping_list.iter()
                     .map(|name| self.price(name))
                     .fold(0.0, |a, b| a + b)
    }
}

fn build_stores() -> Vec<Store> {
    let mut stores = vec![];

    let mut store = Store::new(format!("Rustmart"));
    store.add_item(Item { name: "chocolate", price: 5.0 });
    store.add_item(Item { name: "socks", price: 23.0 });
    store.add_item(Item { name: "plush Mozilla dinosaur", price: 13.0 });
    stores.push(store);

    let mut store = Store::new(format!("Rarget"));
    store.add_item(Item { name: "chocolate", price: 2.5 });
    store.add_item(Item { name: "socks", price: 20.0 });
    store.add_item(Item { name: "plush Mozilla dinosaur", price: 20.0 });
    stores.push(store);

    stores
}

fn main() {
    let stores = build_stores();

    // The Arc (reference counting) appears useless, but actually
    // helps us avoid copying. Without using Arc, the `let
    // shopping_list` below to give each thread their own copy would
    // need to copy the entire vector. With Arc, we instead just copy
    // a reference to the vector.
    let shopping_list = vec!["chocolate", "plush Mozilla dinosaur"];
    let shopping_list = Arc::new(shopping_list);

    // Concurrency helpers
    let (csnd, crcv) : (Sender<StoreResult>, Receiver<StoreResult>) = channel();
    let mut handles = vec![];

    // Spawn a worker for each store
    for store in stores {
        let shopping_list = shopping_list.clone();
        let csnd = csnd.clone();
        // `move` here means move captured variables into the closure,
        // don't copy.
        handles.push(thread::spawn(move || {
            let sum = store.total_price(&shopping_list);
            csnd.send((store.name, sum)).unwrap();
        }));
    }

    // // Goal: join the threads here!
    // let mut best : Option<String> = None;
    // let mut best_price = INFINITY;
    // for h in handles {
    //     match h.join() {
    //         Ok((s,p)) => if p < best_price { best = Some(s) },
    //         Err(_)    => {},
    //     }
    // }
    // println!("--> Go to {}!", best.unwrap());

    // Extra credit: rewrite to use channels or mutexes.
    let (best, _) = crcv.recv().unwrap();
    println!("--> Go to {}!", best);
}
