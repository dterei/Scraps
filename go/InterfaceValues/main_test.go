package main

import (
	"syscall"
	"testing"
)

func BenchmarkUnsafeErr(b *testing.B) {
	num  := uintptr(16)
	errn := syscall.Errno(num)
	err  := error(errn)

	for i := 0; i < b.N; i++ {
		unsafeErr(err)
	}
}

func BenchmarkSafeErr(b *testing.B) {
	num  := uintptr(16)
	errn := syscall.Errno(num)
	err  := error(errn)

	for i := 0; i < b.N; i++ {
		safeErr(err)
	}
}
