// package is basically a namespace, can refer to anything else
// in the package regardless of what file it is in, and everything
// in a package is visible to the whole package
package namespace

func A() int {
  return d()
}

func B() rune {
  return rune(C())
}

func X() *s {
  x := new(s)
  return x
}

// type s is abstract in that it can't be constructed outside
// this package
type s struct {
  // however, field settings determine if it is opaque or not,
  // so here, A is public but b, c, d aren't.
  A int
  b int
  c int
  d int
}

