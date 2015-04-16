package main

import "fmt"
import "time"

const N = 10000000

func time_overhead() {
	min := 100 * time.Hour

	start := time.Now()
	for i := 0; i < N; i++ {
		past := time.Now()
		now := time.Now()
		if p := now.Sub(past); p < min {
			min = p
		}
	}
	end := time.Now()

	fmt.Printf("Min: %v\n", min)
	fmt.Printf("Avg: %v\n", end.Sub(start)/N)
}

func main() {
	time_overhead()
}
