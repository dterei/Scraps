// Wanted to test how many rolls of a dice needed on average to hit a certain
// number -- thinking about Bitcoin hash mining.
//
// We're defining "average" to be that 50% of the time you find the number with
// less than this many rolls and 50% of the time with more than this many rolls.
package main

import "fmt"
import "crypto/rand"
import "math/big"
import "sort"

func main() {
	var results []int
	for i := 0; i < 1000000; i++ {
		r := findLessThan(1, 6)
		results = append(results, r)
	}

	sort.Sort(sort.IntSlice(results))

	i := 0
	for bucket := 0; i < len(results); bucket++ {
		count := 0
		for i < len(results) && bucket == results[i] {
			count++
			i++
		}
		fmt.Printf("%d = %d\n", bucket, count)
	}

	median := 4
	firstHalf, secondHalf := 0, 0
	for _, r := range results {
		if r <= median {
			firstHalf++
		} else {
			secondHalf++
		}
	}
	fmt.Printf("\nMedian: %d\n", median)
	fmt.Printf("First Half: %d (%f)\n", firstHalf, float64(firstHalf) / float64(len(results)))
	fmt.Printf("Second Half: %d (%f)\n", secondHalf, float64(secondHalf)/ float64(len(results)))

	rolls := 0
	for _, r := range results {
		rolls += r
	}
	fmt.Printf("\nAverage Rolls: %f\n", float64(rolls) / float64(len(results)))
}

func findLessThan(under, rangeEnd int64) int {
	for i := 1;; i++ {
		n, _ := rand.Int(rand.Reader, big.NewInt(rangeEnd))
		if n.Int64() < under {
			return i
		}
	}
}
