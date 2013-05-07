// demonstrating the alternative less known function call syntax style golang
// supports.
package main

import (
  "fmt"
)

type I interface { F(int) }

type A struct {}
func (A) F(int) {}

func main() {
  fmt.Printf("hello world\n")

  // normal call syntax (method)
  var va A = A{}
  va.F(1)

  // alternative call syntax (function)
  A.F(va, 1)

  // interface call syntax (method)
  var i I = va
  i.F(1)

  // alternative call syntax (function)
  I.F(va, 1)
}

