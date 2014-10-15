package main

import "fmt"

type X struct {
  ff func()
  x  bool
}

func (this *X) g(x int) {
  fmt.Printf("first! (%d)\n", x)
  this.ff = this.h
}

func (this *X) h() {
  fmt.Printf("fuck...\n")
}

func main() {
  x := &X{nil, true}
  x.ff = func() { x.g (4) }
  x.ff()
  x.ff()
}

