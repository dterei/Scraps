package main

import (
	"fmt"
	"net"
	"os"
	"strconv"
)

const PORT = 8889
const DATA = "HALO"

func main() {
	servAddr := "127.0.0.1:" + strconv.Itoa(PORT)
	tcpAddr, err := net.ResolveTCPAddr("tcp", servAddr)
	if err != nil {
		println("ResolveTCPAddr failed:", err.Error())
		os.Exit(1)
	}

	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	if err != nil {
		println("Dial failed:", err.Error())
		os.Exit(1)
	}

	fmt.Printf("Client Address: %v\n", conn.LocalAddr())
	fmt.Printf("Server Address: %v\n", conn.RemoteAddr())

	_, err = conn.Write([]byte(DATA))
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}

	println("write to server = ", DATA)

	reply := make([]byte, 1024)

	_, err = conn.Read(reply)
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}

	println("reply from server=", string(reply))

	conn.Close()
}
