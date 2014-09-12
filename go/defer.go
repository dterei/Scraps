package main

import "fmt"
import "math/rand"

func main() {
  lim := rand.Intn(6) + 1
  for i := 0; i < lim; i++ {
    defer fmt.Printf("In %d...\n", i) // guaranteed finalizer for main()
    if i == 2 { defer panic("Help!") } // run just as any other panic.
  }
  panic("stop") // queued defer statements before the panic still run.
}

