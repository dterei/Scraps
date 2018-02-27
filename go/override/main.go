package main

// test overriding a method interface in a struct inheriting another struct

import (
	"fmt"
)

type I1 interface {
	SetX(int) int
	GetX() int
}

type S1 struct {
	x int
}

func (s *S1) SetX(x int) int {
	s.x = x
	return s.x
}

func (s *S1) GetX() int {
	return s.x
}

type S2 struct {
	S1
	old int
}

func (s *S2) SetX(x int) int {
	s.old = s.GetX()
	s.S1.SetX(x)
	return s.GetX()
}

func (s *S2) OldX() int {
	return s.old
}

func testOverride(si I1) {
	si.SetX(1)
	si.SetX(2)

	fmt.Printf("New X: %d\n", si.GetX())
}

func main() {
	fmt.Println("hello world")

	s2 := S2{}
	s2.SetX(1)
	s2.SetX(2)

	fmt.Printf("New X: %d\n", s2.GetX())
	fmt.Printf("Old X: %d\n", s2.OldX())

	// Works as a struct, but S2 doesn't implement the I1 interface actually... 
	// testOverride(s2)
}
