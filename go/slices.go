package main

/*
  Try to see how slices and sharing behave. No sync so not safe but should
  be fine for testing.
*/

import (
  "fmt"
  // "time"
)

func rmSlice(slice []int) {
  // fmt.Println("(rmSlice) slice[0]:", slice[0])
  // slice[0] = 9
  // fmt.Println("(rmSlice) slice[0]:", slice[0])

  /* fmt.Println("(rmSlice) slice len:", len(slice)) */
  // slice2 := append(slice, 12)
  // slice2[0] = -1
  // fmt.Println("(rmSlice) slice[0]:", slice[0])
  // fmt.Println("(rmSlice) slice2[0]:", slice2[0])
  // fmt.Println("(rmSlice) slice len:", len(slice))
  // fmt.Println("(rmSlice) slice2 len:", len(slice2))

  slice3 := append(slice[:4], slice[6:]...)

  fmt.Printf("(rmSlice) slice: ")
  for i := 0; i < len(slice); i++ {
    fmt.Printf("%d ", slice[i])
  }
  fmt.Printf("\n")

  fmt.Printf("(rmSlice) slice3: ")
  for i := 0; i < len(slice3); i++ {
    fmt.Printf("%d ", slice3[i])
  }
  fmt.Printf("\n")
  // slice3[5] = 99

  // fmt.Println("(rmSlice) slice[6]:", slice[6])
  // fmt.Println("(rmSlice) slice2[7]:", slice2[7])
  // fmt.Println("(rmSlice) slice3[6]:", slice3[5])
  fmt.Println("(rmSlice) slice len:", len(slice))
  // fmt.Println("(rmSlice) slice2 len:", len(slice2))
  fmt.Println("(rmSlice) slice3 len:", len(slice3))
}

func main() {
  s1 := make([]int, 0, 5)
  // for i := 0; i < 10; i++ {
  //   s1 = append(s1, i)
  // }

  fmt.Println(s1)

  s2 := append(s1, 1, 2)

  fmt.Println(s1)
  fmt.Println(s1[0:cap(s1)])

  s3 := append(s2, 3, 4, 5, 6)

  fmt.Println(s1)
  fmt.Println(s1[0:cap(s1)])

  fmt.Println(s2)
  fmt.Println(s2[0:cap(s2)])

  fmt.Println(s3)
  fmt.Println(s3[0:cap(s3)])


  // fmt.Println("(main) slice[0]:", slice[0])
  // go rmSlice(slice)
  // fmt.Println("(main) slice len:", len(slice))
  //
  // time.Sleep(4 * time.Second)
  // fmt.Println("(main) slice len:", len(slice))
  // fmt.Println("(main) slice[0]:", slice[0])
}

