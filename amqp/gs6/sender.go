package main

// Multiple senders
// Multiple receivers
// Receiver replies to sender - needs dynamic queue
// Default exchange
// Round-robbin routing
// Immediate Acknowledgment

import (
	"fmt"
	"time"

	"github.com/streadway/amqp"
)

const (
	broker       = "amqp://localhost"
	exchange     = ""
	sendQueue    = "gs6-send-queue"
	durable      = false
	autoDelete   = true
	exclusive    = false
	nowait       = false
	internal     = false
	deliveryMode = 0
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

	sendQ, err := chn.QueueDeclare(sendQueue, durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't declare ampq send queue: %v\n", err)
		return
	}
	fmt.Printf("Send queue declared: %s (%d, %d)\n", sendQ.Name, sendQ.Consumers, sendQ.Messages)

	// not-durable, auto-delete, exclusive
	replyQ, err := chn.QueueDeclare("", false, true, true, false, nil)
	if err != nil {
		fmt.Printf("Couldn't declare ampq receive queue: %v\n", err)
		return
	}
	fmt.Printf("Reply queue declared: %s (%d, %d)\n", replyQ.Name, replyQ.Consumers, replyQ.Messages)

	// consume replies
	go func() {
		fmt.Printf("Receive goroutine started...\n")

		recv, err := chn.Consume(replyQ.Name, "", true, true, false, false, nil)
		if err != nil {
			fmt.Printf("Couldn't open amqp queue for receiving: %v\n", err)
			return
		}

		i := 0
		for msg := range recv {
			fmt.Printf("[%03d] msg: %s\n  - AppID: %s\n  - Reply: %s\n  - MsgID: %s\n",
				i, msg.Body, msg.AppId, msg.ReplyTo, msg.MessageId)
			i++
		}
	}()

	// send
	for i := 0; ; i++ {
		err = chn.Publish(exchange, sendQueue, false, false, amqp.Publishing{
			// AMQP meaningful
			DeliveryMode: deliveryMode,
			UserId:       "",

			// following fields appear to be user controlled and untouched by AMQP
			Body:      []byte(fmt.Sprintf("[x] hello world [%03d]", i)),
			AppId:     "gs6",
			ReplyTo:   replyQ.Name,
			MessageId: fmt.Sprintf("%s-%d", replyQ.Name, i),
		})
		if err != nil {
			fmt.Printf("Couldn't send message: %v\n", err)
			return
		}
		time.Sleep(500 * time.Millisecond)
	}
}
