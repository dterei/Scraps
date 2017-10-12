package main

import "context"
import "fmt"
import "time"

func main() {
	ctx, cancel := context.WithCancel(context.Background())

	fmt.Printf("start: ")
	go keepAlive(ctx)

	time.Sleep(6 * time.Second)
	cancel()
	time.Sleep(1 * time.Second)
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
