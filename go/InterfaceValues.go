package main

import (
	"fmt"
	"syscall"
	"unsafe"
)

func main() {
	// uinptr -> Errno -> error
	var num uintptr
	var errn syscall.Errno
	var err error

	num  = 16
	errn = syscall.Errno(num)
	err  = errn

	fmt.Println("Num:", num)
	fmt.Println("Errno:", errn)
	fmt.Println("Error:", err)

	// can we unsafe cast to unwrap all the interface layers? Or is the value in
	// memory different now?
	// No! We have a new layer of indirection...
	var num2 uintptr
	var p1 = (uintptr)(unsafe.Pointer(&err))
	var p2 = (*uintptr)(unsafe.Pointer(p1+8))
	num2 = *(*uintptr)(unsafe.Pointer(*p2))
	fmt.Println("Num2:", num2)
}
