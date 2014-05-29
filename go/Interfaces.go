package main

import "fmt"

// ARGH! Go interfaces / method sets are only open within a pacakge. I.e., I
// can't add methods to bool, i can only add methods to types defined in this
// package.
type ToInt interface {
  toint() int
}

type MBool struct {
  bool
}

func (this MBool) toint() int {
  if (this.bool) { return 1 } else { return 0 }
}

func main() {
  b := MBool{true}
  x := false
  fmt.Printf("b = %d, x = %d\n", b.toint(), x.toint())
}

