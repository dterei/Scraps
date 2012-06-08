package main

import "fmt"
import "net"

func main() {
  server := "127.0.0.1:8080"

  addr, err := net.ResolveTCPAddr("tcp", server)
  if err == nil {
    fmt.Printf("error parsing address: %s\n", err)
  }

  serverConn, err := net.DialTCP("tcp", nil, addr)

  if err != nil {
    fmt.Printf("error connecting: %s\n", err)
  }

  if serverConn != nil {
    err = serverConn.Close()
    if err != nil {
      fmt.Printf("error closing: %s\n", err)
    }
  }

  fmt.Printf("Bad close doesn't matter!\n")
}

