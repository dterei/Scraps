#![allow(dead_code)]

// A trait for printing data with indentation
trait Print {
    // A default method for ignoring indentation
    fn print(&self) {
        self.print_with_indent(0);
    }

    // Print the data with the given indentation level
    fn print_with_indent(&self, indent: usize);
}

// A helper function that produces `amount` indentation
fn print_indent(amount: usize) {
    print!("{:width$}","", width = amount);
}

impl Print for u64 {
    fn print_with_indent(&self, indent: usize) {
        print_indent(indent);
        println!("{}", self)
    }
}

impl Print for char {
    fn print_with_indent(&self, indent: usize) {
        // Goal: print the given character at the given indentation level,
        // surrounding the character by single quotes
    }
}

impl<T: Print> Print for Vec<T> {
    fn print_with_indent(&self, indent: usize) {
        // Goal: print the contents of the vector, indenting items by two
        // additional characters, and surrounding the vector by `Vec [` and `]`.
        for item in self {}
    }
}

fn main() {
    /* Make this compile and print:
     *
     * Vec [
     *   'h'
     *   'i'
     * ]
     *
     */

    let w: Vec<char> = vec!['h', 'i'];
    w.print();

    /* Make this compile and print:
     *
     * Vec [
     *   Vec [
     *     2
     *   ]
     *   Vec [
     *     3
     *     5
     *   ]
     * ]
     *
     */

    let mut v: Vec<Vec<u64>> = vec![];
    v.push(vec![2]);
    v.push(vec![3, 5]);
    v.print();
}
