package main

// safe   -- 15.9ns/op
// unsafe --  1.6ns/op

import (
	"fmt"
	"syscall"
	"unsafe"
)

// can we unsafe cast to unwrap all the interface layers? Or is the value in
// memory different now? No! We have a new layer of indirection...
func unsafeErr(err error) uintptr {
	if err != nil {
		p1 := (uintptr)(unsafe.Pointer(&err))
		p2 := (*uintptr)(unsafe.Pointer(p1+8))
		return *(*uintptr)(unsafe.Pointer(*p2))
	} else {
		return 0
	}
}

// Safe way, type assertion
func safeErr(err error) uintptr {
	return uintptr(err.(syscall.Errno))
}

func main() {
	// uinptr -> Errno -> error
	num  := uintptr(16)
	errn := syscall.Errno(num)
	err  := error(errn)

	fmt.Println("Num:", num)
	fmt.Println("Errno:", errn)
	fmt.Println("Error:", err)

	fmt.Println("Unsafe way:", unsafeErr(err))
	fmt.Println("Safe   way:", safeErr(err))
}
