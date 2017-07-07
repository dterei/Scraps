// Simple TCP echo server in go.
package main

import (
	"bufio"
	"fmt"
	"net"
	"strconv"
)

const PORT = 8888

func main() {
	server, err := net.Listen("tcp", ":"+strconv.Itoa(PORT))
	if err != nil {
		panic("couldn't start listening: " + err.Error())
	}

	for {
		client, err := server.Accept()
		if err != nil {
			fmt.Printf("couldn't accept new connection: " + err.Error())
			continue
		}
		go handleConn(client)
	}
}

func handleConn(client net.Conn) {
	fmt.Printf("Client Address: %v\n", client.RemoteAddr())
	fmt.Printf("Server Address: %v\n", client.LocalAddr())
	b := bufio.NewReader(client)
	for {
		line, err := b.ReadBytes('\n')
		if err != nil { // EOF, or worse
			break
		}
		client.Write(line)
	}
}
