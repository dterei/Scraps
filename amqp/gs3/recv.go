package main

// Single sender
// Multiple receivers - round-robin
// Durable
// Delayed acknowledgment

import (
	"fmt"
	"time"

	"github.com/streadway/amqp"
)

const (
	broker = "amqp://localhost"
	queue1 = "gs3-queue"

	// Should queue persist beyond broker restart?
	durable = true

	// Should queue be deleted once all clients have disconnected?
	autoDelete = false

	exclusive = false
	nowait    = false
	nolocal   = false // not supported
	autoAck   = false
)

func main() {
	conn, err := amqp.Dial(broker)
	if err != nil {
		fmt.Printf("Couldn't open amqp connection: %v\n", err)
		return
	}
	defer conn.Close()

	chn, err := conn.Channel()
	if err != nil {
		fmt.Printf("Couldn't open amqp channel: %v\n", err)
		return
	}
	defer chn.Close()

	qq, err := chn.QueueDeclare(queue1, durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue: %v\n", err)
		return
	}
	fmt.Printf("Queue declared: %s (%d, %d)\n", qq.Name, qq.Consumers, qq.Messages)

	recv, err := chn.Consume(queue1, "", autoAck, exclusive, nolocal, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue for receiving: %v\n", err)
		return
	}

	var msg amqp.Delivery
	lastAckd := 0
	i := 1

	go func() {
		for {
			if len(msg.Body) > 0 && lastAckd < i {
				fmt.Printf("acking up to %d\n", i)
				msg.Ack(true)
				lastAckd = i
			}
			time.Sleep(5000 * time.Millisecond)
		}
	}()

	for msg = range recv {
		fmt.Printf("[%03d] msg: %v\n", i, msg)
		i++
	}

	fmt.Printf("messages done...\n")
}
