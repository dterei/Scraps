package main

import (
  "encoding/binary"
  "fmt"
  "log"
  "net"
  "net/rpc"
)

func main() {
  Globby()
  RawwGo()
  RawwGo2()
  RawwC()
}

func Globby() {
  client, err := rpc.Dial("tcp", "localhost:1234")
  if err != nil {
    log.Fatal("dialing:", err)
  }

  x := new(struct{})
  var y struct{}
  err = client.Call("Gobby.Noop", x, &y)
  if err != nil {
    log.Fatal("gobby error:", err)
  }
  fmt.Printf("Gobby: Noop\n")

  args := &Args{7,8}
  var reply int64
  err = client.Call("Gobby.Multiply", args, &reply)
  if err != nil {
    log.Fatal("gobby error:", err)
  }
  fmt.Printf("Gobby: %d*%d = %d\n", args.A, args.B, reply)

  arg := "hello world how are you?"
  var r string
  err = client.Call("Gobby.ToUpper", arg, &r)
  if err != nil {
    log.Fatal("gobby error:", err)
  }
  fmt.Printf("Gobby.ToUpper: %s => %s\n", arg, r)
}

func RawwGo() {
  conn, err := net.Dial("tcp", "localhost:1235")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 6
  var y int64 = 8
  var z int64
  binary.Write(conn, binary.BigEndian, &x)
  binary.Write(conn, binary.BigEndian, &y)
  binary.Read(conn, binary.BigEndian, &z)
  fmt.Printf("RawGo_refl: %d*%d = %d\n", x, y, z)
  conn.Close()
}

func RawwGo2() {
  conn, err := net.Dial("tcp", "localhost:1236")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 6
  var y int64 = 8
  var z int64
  binary.Write(conn, binary.BigEndian, &x)
  binary.Write(conn, binary.BigEndian, &y)
  binary.Read(conn, binary.BigEndian, &z)
  fmt.Printf("RawGo_hand: %d*%d = %d\n", x, y, z)
  conn.Close()
}

func RawwC() {
  conn, err := net.Dial("tcp", "localhost:1237")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 6
  var y int64 = 8
  var z int64
  binary.Write(conn, binary.BigEndian, &x)
  binary.Write(conn, binary.BigEndian, &y)
  binary.Read(conn, binary.BigEndian, &z)
  fmt.Printf("RawC: %d*%d = %d\n", x, y, z)
  conn.Close()
}

