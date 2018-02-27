package main

// testing defer scope - function scope only, no block scope

import "fmt"
import "math/rand"

func deferScope() {
	fmt.Printf("deferScope start...\n")
  lim := rand.Intn(6) + 1
  for i := 0; i < lim; i++ {
		fmt.Printf("Loop %d\n", i)
		// defers on run when function returns, not when a block exits
    defer fmt.Printf("In %d...\n", i) // guaranteed finalizer for main()
    if i == 2 { defer panic("Help!") } // run just as any other panic.
  }
	fmt.Printf("deferScope exit...\n")
}

func main() {
	deferScope()
  panic("stop") // queued defer statements before the panic still run.
}

