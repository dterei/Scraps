package main

import "context"
import "fmt"
import "time"

func main() {
	// This is OK actually, only creates one ticker...
	fmt.Printf("start...\n")
	for now := range debugTick(5 * time.Second) {
		fmt.Printf("- %s\n", now)
	}

	ctx, cancel := context.WithCancel(context.Background())

	fmt.Printf("start: ")
	go keepAlive(ctx)

	time.Sleep(6 * time.Second)
	cancel()
	time.Sleep(1 * time.Second)
}

func debugTick(d time.Duration) <- chan time.Time {
	fmt.Printf("debugTick\n")
	return time.Tick(d)
}

func keepAlive(ctx context.Context) {
	tick := time.NewTicker(1 * time.Second)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			fmt.Printf(" keep alive over!\n")
			return
		case <- tick.C:
			fmt.Printf(".")
		}
	}
}
