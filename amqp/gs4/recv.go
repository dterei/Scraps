package main

// Single sender
// Multiple receivers
// Fanout exchange
// Non-Durable
// Immediate Acknowledgment

import (
	"fmt"

	"github.com/streadway/amqp"
)

const (
	broker     = "amqp://localhost"
	exchange   = "gs4-exchage"
	key        = ""
	durable    = false
	autoDelete = true
	exclusive  = true // since new temporal queues - helps with deletion
	nowait     = false
	nolocal    = false // not supported
	autoAck    = true
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

	qq, err := chn.QueueDeclare("", durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue: %v\n", err)
		return
	}
	fmt.Printf("Queue declared: %s (%d, %d)\n", qq.Name, qq.Consumers, qq.Messages)

	chn.QueueBind(qq.Name, key, exchange, nowait, nil)

	recv, err := chn.Consume(qq.Name, "", autoAck, exclusive, nolocal, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue for receiving: %v\n", err)
		return
	}

	i := 1
	for msg := range recv {
		fmt.Printf("[%03d] msg: %s\n", i, msg.Body)
		i++
	}
}
