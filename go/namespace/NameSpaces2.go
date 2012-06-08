package namespace

func C() int {
  return 3
}

func d() int {
  return 4
}

func E() int {
  return d()
}

func F() int {
  x := new(s)
  return x.A
}

