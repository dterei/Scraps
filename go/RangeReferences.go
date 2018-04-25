package main

import "fmt"

// A is a example struct.
type A struct {
	x int
}

func main() {
	// make example slice
	as := make([]A, 0)
	for i := 0; i < 5; i++ {
		as = append(as, A{i})
	}

	// try to make a new slice that references values stored in example slice
	as2 := make([]*A, len(as))
	for i, a := range as {
		// BUG: 'a' is a shared variable with for-loop scope storing a copy of a[i]
		// updated each loop iteration. Go doesn't support range loops that return a
		// reference like C++ does sadly. Taking the reference of 'a' takes the
		// reference to the local variable, (in C++ would be a dangling pointer, but
		// Go with GC will lift 'a' to the heap). So all as2[i] store a reference to
		// this same memory location, which will have the value of a[len(a)-1].
		as2[i] = &a
	}

	// print out slices
	fmt.Printf(" A1's: %v\n", as)
	fmt.Printf(" A2's: %v\n", as2)
	fmt.Printf("*A2's: [")
	for i, a := range as2 {
		if i+1 == len(as) {
			fmt.Printf("%v", *a)
		} else {
			fmt.Printf("%v ", *a)
		}
	}
	fmt.Printf("]\n")
}
