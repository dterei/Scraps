package main

import (
  "container/list"
  "fmt"
)

type A struct {
  x int
}

func main() {
  var l *list.List
  l = list.New()

  a := &A{123}

  m := uint64(uint32(2<<16) * uint32(2<<13))
  e := uint64(2<<29)

  fmt.Printf("m = e || %v = %v\n", m, e)

  fmt.Printf("\nBack: %v\n", l.Back())

  fmt.Println("\ntry move to front with nil...")
  l.MoveToFront((*list.Element)(a))

  fmt.Println("\ntry removing nil from list...")
  l.Remove(nil)
}

