package main

import "fmt"

func main() {
  var rw chan int
  var r <-chan int
  var w chan<- int

  // Go spec currently allows creating a unusable channel as below.
  // Left in spec to keep it smaller (no special case for what make
  // allows or disallows)
  // r = make(<-chan int)

  rw = make(chan int)
  r = rw
  w = rw

  // uninitialized is nil -- will block forever on any communication.
  var block chan int

  go func() {
    for {
      x := <-r
      fmt.Printf("Goroutine got: %d\n", x)
      _ = <-block
    }
  }()

  w<-10

  // race condition as we may print and exit before our goroutine
  // prints.

  fmt.Printf("Starting...\n")
}

