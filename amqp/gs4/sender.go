package main

// Single sender
// Multiple receivers
// Fanout exchange
// Non-Durable
// Immediate Acknowledgment

import (
	"fmt"
	"time"

	"github.com/streadway/amqp"
)

const (
	broker       = "amqp://localhost"
	exchange     = "gs4-exchage"
	key          = ""
	durable      = true
	autoDelete   = false
	nowait       = false
	internal     = false
	deliveryMode = amqp.Persistent
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

	err = chn.ExchangeDeclare(exchange,
		amqp.ExchangeFanout, durable, autoDelete, internal, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't declare ampq exchange: %v\n", err)
		return
	}

	for i := 0; ; i++ {
		err = chn.Publish(exchange, key, false, false, amqp.Publishing{
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
