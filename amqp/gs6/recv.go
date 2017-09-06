package main

// Multiple senders
// Multiple receivers
// Receiver replies to sender - needs dynamic queue
// Default exchange
// Round-robbin routing
// Immediate Acknowledgment

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"github.com/streadway/amqp"
)

const (
	broker     = "amqp://localhost"
	exchange   = ""
	recvQueue  = "gs6-send-queue"
	durable    = false
	autoDelete = true
	exclusive  = false
	nowait     = false
	nolocal    = false // not supported
	autoAck    = true
)

func genUUID() string {
	buf := make([]byte, 8)
	_, err := rand.Read(buf)
	if err != nil {
		panic(err)
	}
	return hex.EncodeToString(buf)
}

func main() {
	myuuid := genUUID()
	fmt.Printf("Receiver %s started...\n", myuuid)
	
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

	sendQ, err := chn.QueueDeclare(recvQueue, durable, autoDelete, exclusive, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue: %v\n", err)
		return
	}
	fmt.Printf("Recv queue declared: %s (%d, %d)\n", sendQ.Name, sendQ.Consumers, sendQ.Messages)

	recv, err := chn.Consume(recvQueue, myuuid, autoAck, exclusive, nolocal, nowait, nil)
	if err != nil {
		fmt.Printf("Couldn't open amqp queue for receiving: %v\n", err)
		return
	}

	i := 0
	for msg := range recv {
		fmt.Printf("[%03d] msg: %s\n  - AppID: %s\n  - Reply: %s\n  - MsgID: %s\n",
			i, msg.Body, msg.AppId, msg.ReplyTo, msg.MessageId)
		i++

		// send reply to specific sender
		err = chn.Publish(exchange, msg.ReplyTo, false, false, amqp.Publishing{
			Body:      []byte(fmt.Sprintf("[x] goodbye world [%03d]", i)),
			AppId:     "gs6",
			ReplyTo:   myuuid,
		})
		if err != nil {
			fmt.Printf("Couldn't send reply message: %v\n", err)
			return
		}
	}

	fmt.Printf("messages done...\n")
}
