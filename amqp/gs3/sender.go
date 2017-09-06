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

	// If queue is durable AND message is persistent, then it will survive a
	// broker restart.
	// If queue is durable AND message transient, it won't survive a broker
	// restart.
	//
	// If queue is not-durable, messages won't survive a broker restart
	//
	deliveryMode = amqp.Persistent // amqp.Transient
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

	for i := 0;; i++ {
		err = chn.Publish("", queue1, false, false, amqp.Publishing{
			Body:         []byte(fmt.Sprintf("[x] hello world [%03d]", i)),
			DeliveryMode: deliveryMode,
		})
		if err != nil {
			fmt.Printf("Couldn't send message: %v\n", err)
			return
		}
		time.Sleep(500 * time.Millisecond)
	}
}
