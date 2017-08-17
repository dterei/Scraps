package main

import (
	"bytes"
	"fmt"
	"io"
)

// Go interface lift the type, so they introduce another level of `nil`. Storing
// anything (i.e., any concrete type) into an interface, even if that is `nil`,
// makes the interace itself non-nil.
//
// Operationally, we can model an interface at runtime as a tuple of
// (type, value), a "nil" interface value is `(nil, nil)`, while an interface
// storing a nil concrete type is `(type, nil)`.
func main() {
	nilInterface()
	nonNilInterface()
	nilnilInterface()
}

func nonNilInterface() {
	var b *bytes.Buffer
	var r io.Reader = b
	fmt.Printf("nonNilInterface: %v\n", r == nil)
}

func nilInterface() {
	var r io.Reader
	fmt.Printf("nilInterface: %v\n", r == nil)
}

func nilnilInterface() {
	var r1 io.Reader
	var r2 io.Reader = r1
	fmt.Printf("nilnilInterface: %v\n", r2 == nil)
}
