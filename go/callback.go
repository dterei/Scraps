package main

import "fmt"

func applySimple(f func(int), i int) {
  f(i)
}

func main() {
  applySimple(func (x int) {
    fmt.Printf("X is %d\n", x)
  }, 10)
}

