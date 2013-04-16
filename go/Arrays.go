package main

import "fmt"

var x [10]int

func array(i int) {
  fmt.Printf("X[%d] = %d\n", i, x[i])
}

func main() {
  array(10)
}

