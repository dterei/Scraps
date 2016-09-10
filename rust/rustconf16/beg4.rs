pub fn main() {
    let (mut str1, str2) = two_words();
    join_words(&mut str1, &str2);
    println!("concatenated string is {:?}", str1);
    str1 = join_words_orig(str1, &str2);
    println!("concatenated string is {:?}", str1);
}

fn two_words() -> (String, String) {
    (format!("fellow"), format!("Rustaceans"))
}

/// Concatenate `suffix` onto the end of `prefix`.
fn join_words_orig(mut prefix: String, suffix: &String) -> String {
    prefix.push(' '); // separate the words with a space
    for ch in suffix.chars() {
        prefix.push(ch);
    }
		prefix
}

/// Concatenate `suffix` onto the end of `prefix`.
fn join_words(mut prefix: &mut String, suffix: &String) {
    prefix.push(' '); // separate the words with a space
    for ch in suffix.chars() {
        prefix.push(ch);
    }
}

// Challenge: Convert `join_words` to use borrowing, not ownership.
// The new function should mutate `prefix` in place, and should not
// take ownership of `suffix`.
//
// Hint: If you'd like a hint as to how to proceed, open
// <http://rust-tutorials.com/exercises/hint-mutable-borrow-1.html>.

// Question: Now that you've converted `join_words`, what happens if
// you call `join_words` using the same string for `prefix` and
// `suffix`? Why?
//
// We get an error that we are trying to borrow str1 both as mutable
// and immutable, as we are trying to use it twice in one scope with
// conflicting constraints.
