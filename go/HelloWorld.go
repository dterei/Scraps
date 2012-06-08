package main

import "fmt"
import "./namespace"

func main() {
  fmt.Printf("Hello World!\n")
  fmt.Printf("%d %d %d %d\n", namespace.A(), namespace.B(), namespace.C(), namespace.E())
  fmt.Println(namespace.X())

  // fails as s isn't exported
  // x := new(namespace.s)
  y := namespace.X()
  fmt.Println(y.A)

  fmt.Println(namespace.F())
}

