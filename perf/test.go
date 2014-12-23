package main

import (
  "fmt"
  "math/rand"
  "mod"
  "syscall"
  "time"
)

const CALLS = 65536

var sum uint64

func A112() { sum += 0 }
func b112() { sum += 1 }
func c112() { sum += 2 }
func d112() { sum += 3 }
func e112() { sum += 4 }
func f112() { sum += 5 }
func g112() { sum += 6 }
func k112() { sum += 7 }
func i112() { sum += 8 }
func j112() { sum += 9 }

func run() {
  for x := 0; x < CALLS; x++ {
    switch (rand.Intn(10)) {
    case 0: A112(); break;
    case 1: mod.B112(); break;
    case 2: c112(); break;
    case 3: d112(); break;
    case 4: e112(); break;
    case 5: f112(); break;
    case 6: g112(); break;
    case 7: k112(); break;
    case 8: i112(); break;
    case 9: j112(); break;
    }
  }
}

func main() {
    p := syscall.Getpid()
    fmt.Printf("pid: %d\n", p)

    start := time.Now()
    run()

    fmt.Printf("took: %s\n", time.Since(start))
    fmt.Printf("sum: %d\n", sum)
}

