package main

/*
 Go interfaces support a subtyping relation. However, the Go Language Reference
 never explicitly mentions subtyping, just 'subsets'. So lets play around.
*/

import "fmt"

type A interface {
  a() int
}

// B is a subtype of A
type B interface {
  A
  b() int
}

// C is a subtype of B
type C interface {
  B
  c() int
}

// A2 is a new top level type
type A2 interface {
  a2() int
}

// B2 <: A && B2 <: A2
type B2 interface {
  A
  A2
  b() int
}

type TA struct {}
func (this TA) a() int { return 0 }
func (this TA) b() int { return 1 }

func main() {
  t := TA{}
  var a A
  var b B

  // 'TA' is a subtype of 'B'
  // TA <: B
  // 't::TA' can be used anywhere that 'b::B' is expected
  b = t
  // effectively means that the method set of 'TA' is a superset of 'B'. (i.e.,
  // kind of confusing that we say 'TA' is a subtype of 'A' when its method set
  // is a superset. *However* if we think in terms of value sets defining
  // members of a type, then the set of values of 'B' is a superset of the set
  // of values of 'TA' since all 'TA' are in 'B'). So subtying relates to method
  // sets in terms of the reverse direction.

  // So if the method set of type 'X' is a subset of method set of type 'Y' then
  // 'Y' is a subtype of 'X', Y <: X (i.e., the requirements on X are less than
  // Y, so X is more relaxed and contains more values...).

  // TA <: A
  a = t
  // B <: A
  a = b

  // A </: B
  //b = a

  // TA <: B <: A
  fmt.Printf("%d\n", a.a())
  return
}

