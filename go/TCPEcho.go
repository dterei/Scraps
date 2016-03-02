// Simple TCP echo server in go.
package main

import (
    "net"
    "bufio"
    "strconv"
    "fmt"
)

const PORT = 8888

func main() {
	server, err := net.Listen("tcp", ":" + strconv.Itoa(PORT))
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
	b := bufio.NewReader(client)
	for {
		line, err := b.ReadBytes('\n')
		if err != nil { // EOF, or worse
			break
		}
		client.Write(line)
	}
}
