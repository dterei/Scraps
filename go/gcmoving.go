// Can we operate over pointer values? i.e., the address of memory? This would
// create issues should Go ever move to a moving GC.
//
package main

// Pointer operations
// - Allowed: ==, !=
// - Not allowed: +, -, *, %, ^, |, <, <=, >, >=

import "fmt"

func main() {
	fmt.Println("hello world")

	x := 0
	y := 0

	fmt.Println("X: ", x, "&X:", &x)
	fmt.Println("Y: ", x, "&Y:", &x)
	fmt.Println("X==Y: ", x==y, "&X==&Y:", &x==&y)
	z := &x
	fmt.Println("Z: ", z, "&Z:", &z)
}
