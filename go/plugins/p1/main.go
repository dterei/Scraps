package main

import "fmt"

// init is called on plugin load.
func init() {
	fmt.Printf("init for plugin called...\n")
}

// V is a simple plugin variable.
var V int

// F is a simple plugin function.
func F() {
	fmt.Printf("Hello, number %d\n", V)
}
