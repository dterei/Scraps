package main

import (
  "fmt"
  "runtime"
)

var i *int

func f(x int) {
  pc, file, line, ok := runtime.Caller(0)
  if !ok {
    panic("runtime.Caller not OK!")
  }
  fmt.Printf("pc = %d, file = %s, line = %d\n", pc, file, line)
  fmt.Printf("f %d\n", x)
}

func final(ii *int) {
  fmt.Printf("finalizer called on int %d\n", *ii)
}

func assign() {
  ii := 10
  runtime.SetFinalizer(&ii, final)
  i = &ii
}

func main() {
  assign()
  i = nil
  runtime.GC()
  // runtime.LockOSThread()
  f(1)
  runtime.GC()
  // runtime.Goexit()
}

