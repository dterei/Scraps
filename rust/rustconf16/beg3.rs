pub fn main() {
    let string = format!("my friend");
    greet(&string);
    greet(&string);
    greet(&string[3..]);
}

// Question: &str vs &std::string::String
fn greet(name: &str) {
    println!("Hello, {}!", name);
}

// Goal #1: Convert `greet` to use borrowing, not ownership, so that
// this program executes without doing any cloning.
//
// Goal #2: Use a subslice so that it prints "Hello, friend" instead of
// "Hello, my friend".
