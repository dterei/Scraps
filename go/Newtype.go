package Newtype

import "fmt"

type A string
type B string

type C struct { string }
type X struct { string }

func foo() string {
  var s string
  var a A
  var b B

  s = "string"
  a = "Hello"
  b = "World"

  // CAN'T DO
  // a = b
  // b = a
  // a = s
  // s = a

  // anonymous struct structuraly equivalent to C & X
  var d struct { string }
  c := C{ "Hey" }
  x := X { "world!" }

  // CAN'T DO
  // c = x

  d = x
  c = d

  fmt.Println(a)
  fmt.Println(b)
  fmt.Println(c)

  return s
}

