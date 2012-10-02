package main

/*
  Testing of semantics of length and capacity of slices and append.

  # Result
  -> slices are basically equivalent to ArrayList (Java). An auto-grown array
     implementation (i.e., Vector in other parlance).
  -> append will grow a slice if needed.
  -> append adds an element at position [len(slice)].
  -> so len is like the 'current number of elements in slice / arraylist'
  -> cap is size of actual underlying array, e.g., how many we can append before
     we'll need to grow the array.
*/

import (
  "fmt"
)

func append10(slice []int) []int {
  for i := 0; i < 10; i++ {
    slice = append(slice, i)
  }

  return slice
}

func printSliceByLen(slice []int) {
  fmt.Printf("  printing slice by len (%d)...\n", len(slice))
  fmt.Printf("  slice[..] = [")
  for i := 0; i < len(slice) - 1; i++ {
    fmt.Printf("%d, ", slice[i])
  }
  fmt.Printf("%d]\n", slice[len(slice) - 1])
}

func printSliceByCap(slice []int) {
  fmt.Printf("  printing slice by cap (%d)...\n", cap(slice))
  fmt.Printf("  slice[..] = [")
  for i := 0; i < cap(slice) - 1; i++ {
    fmt.Printf("%d, ", slice[i])
  }
  fmt.Printf("%d]\n", slice[cap(slice) - 1])
}

func main() {
  // slice with len: 0, cap: 0.
  slice1 := make([]int, 0, 0)

  // slice with len: 0, cap: 10.
  slice2 := make([]int, 0, 10)

  // slice with len: 10, cap: 10.
  slice3 := make([]int, 10, 10)

  // slice with len: 10, cap: 20.
  slice4 := make([]int, 10, 20)

  slice1 = append10(slice1)
  fmt.Println("slice of len: 0, cap: 0")
  printSliceByLen(slice1)
  // XXX: below causes error if left in as cap is outside len range here.
  // printSliceByCap(slice1)

  slice2 = append10(slice2)
  fmt.Println("slice of len: 0, cap: 10")
  printSliceByLen(slice2)
  printSliceByCap(slice2)

  slice3 = append10(slice3)
  fmt.Println("slice of len: 10, cap: 10")
  printSliceByLen(slice3)
  printSliceByCap(slice3)

  slice4 = append10(slice4)
  fmt.Println("slice of len: 10, cap: 20")
  printSliceByLen(slice4)
  printSliceByCap(slice4)

  // slice with len: 10, cap: ? (i.e., what does Go set my cap to?)
  slice5 := make([]int, 10)

  // slice with len: ?, cap: ? (i.e., what does Go set my cap to?)
  var slice6 []int
  // XXX: Below is invalid syntax, make needs a length argument.
  // slice6 := make([]int)

  fmt.Printf("make([]int, 10) => len: %d, cap: %d\n", len(slice5), cap(slice5))
  slice5 = append10(slice5)
  printSliceByLen(slice5)
  printSliceByCap(slice5)

  fmt.Printf("make([]int) => len: %d, cap: %d\n", len(slice6), cap(slice6))
  slice6 = append10(slice6)
  printSliceByLen(slice6)
  // printSliceByCap(slice6)
}

