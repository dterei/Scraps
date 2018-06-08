package main

import "fmt"

type A string

func main() {
	A := A("A")
	B := "B"

	fmt.Printf("A: %s | B: %s\n", A, B)

	fmt.Print("Checking A... ")
	checkType(A)

	fmt.Print("Checking B... ")
	checkType(B)
}

func checkType(a interface{}) {
	switch a.(type) {
	case string:
		fmt.Println("string")
	default:
		fmt.Println("?")
	}

}
