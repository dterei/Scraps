package main

import "fmt"
import "os"
import "syscall"
import "time"

func main() {
	start := time.Now()

	args := os.Args

	if len(args) == 1 {
		now := time.Now()
		_, err := syscall.ForkExec("Exec", []string{"Exec", "dont"}, nil)
		if err != nil {
			fmt.Printf("Err: %v\n", err)
		}
		fmt.Printf("Parent: %v\n", now)
	} else {
		fmt.Printf("Child: %v\n", start)
	}
}
