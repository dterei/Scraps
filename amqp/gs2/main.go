package main

// Simplest sender and receiver over one channel with the unamed exchange.
// Durable queue.

import (
	"fmt"
	"sync"

	"github.com/streadway/amqp"
)

const (
	broker      = "amqp://localhost"
	queue1      = "gs2-queue"
	durable     = true
	autoDelete  = false
	exclusive   = false
	nowait      = false
	nolocal     = false // not supported
	autoAck     = false
	mesgN       = 5
	delUnused   = true
	delNonEmpty = true
)

func main() {
	wg := &sync.WaitGroup{}
	wg.Add(2)

	go sender(wg)
	go receiver(wg)

	wg.Wait()
}

func sender(wg *sync.WaitGroup) {
	defer wg.Wait()

	// connect to AMQP broker - while we could share a single TCP connection in
	// the app, it's recommended that Publish and Consume operate over different
	// TCP connections to avoid publishing cause TCP pushback for consuming.
	conn, err := amqp.Dial(broker)
	if err != nil {
		fmt.Printf("Couldn't open amqp connection: %v\n", err)
		return
	}
	defer conn.Close()

	// channel to communicate with AMQP - channels allow us to multiplex AMPQ
	// messages over a single TCP connection rather than open multiple. Each
	// channel can operate concurrently and independently of all other channels
	// (by and large, they do share the TCP connection so can cause resource
	// competition, congestion control interference... etc).
	chn, err := conn.Channel()
	if err != nil {
		fmt.Printf("Couldn't open amqp channel: %v\n", err)
		return
	}
	defer chn.Close()

	// delete the queue if already exists and drop messages.
	n, err := chn.QueueDelete(queue1, !delUnused, !delNonEmpty, nowait)
	if err != nil {
		fmt.Printf("Couldn't close amqp queue %s: %v\n", queue1, err)

		// if error, channel gets closed, so we reset it here
		chn, _ = conn.Channel()
	}
	fmt.Printf("Messages droped from %s: %d\n", queue1, n)

	// declare a queue to communicate over - just uses the default, unnamed
	// exchange, "".
	qq, err := chn.QueueDeclare(queue1, durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue: %v\n", err)
		return
	}
	fmt.Printf("Queue declared: %s (%d, %d)\n", qq.Name, qq.Consumers, qq.Messages)

	// send some messages on queue1 asynchronously
	for i := 0; i < mesgN; i++ {
		err = chn.Publish("", queue1, false, false, amqp.Publishing{
			Body: []byte(fmt.Sprintf("[x] hello world [%03d]", i)),
		})
		if err != nil {
			fmt.Printf("Couldn't send message: %v\n", err)
		}
	}
}

func receiver(wg *sync.WaitGroup) {
	defer wg.Wait()

	// 1. open connection
	conn, err := amqp.Dial(broker)
	if err != nil {
		fmt.Printf("Couldn't open amqp connection: %v\n", err)
		return
	}
	defer conn.Close()

	// 2. open channel
	chn, err := conn.Channel()
	if err != nil {
		fmt.Printf("Couldn't open amqp channel: %v\n", err)
		return
	}
	defer chn.Close()

	// 3. declare queue
	qq, err := chn.QueueDeclare(queue1, durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't declare amqp queue: %v\n", err)
		return
	}
	fmt.Printf("Queue declared: %s (%d, %d)\n", qq.Name, qq.Consumers, qq.Messages)

	// 4. consume from queu
	recv, err := chn.Consume(queue1, "myuuid", autoAck, exclusive, nolocal, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue for receiving: %v\n", err)
		return
	}

	i := 1
	for msg := range recv {
		fmt.Printf("[%03d] msg: %s\n", i, msg.Body)
		i++
		// 5. ack the message - all messages must be acknowledged
		msg.Ack(true)
	}
}
