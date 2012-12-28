package main

import (
  "encoding/binary"
  "log"
  "net"
  "net/rpc"
  "testing"
)

// HTTP: takes 100,883 ns/op
// RAW : takes  98,969 ns/op
func BenchmarkNoopRpcCall(b *testing.B) {
  client, err := rpc.Dial("tcp", "localhost:1234")
  if err != nil {
    log.Fatal("dialing:", err)
  }

  x := new(struct{})
  var y struct{}

  for i := 0; i < b.N; i++ {
    err = client.Call("Gobby.Noop", x, &y)
    if err != nil {
      log.Fatal("gobby error:", err)
    }
  }
}

// HTTP: takes 101,896 ns/op
// RAW : takes  99,674 ns/op
func BenchmarkIntRpcCall(b *testing.B) {
  client, err := rpc.Dial("tcp", "localhost:1234")
  if err != nil {
    log.Fatal("dialing:", err)
  }

  args := &Args{7,8}
  var reply int

  for i := 0; i < b.N; i++ {
    err = client.Call("Gobby.Multiply", args, &reply)
    if err != nil {
      log.Fatal("gobby error:", err)
    }
  }
}

// HTTP: takes 108,500 ns/op
// RAW : takes 101,908 ns/op
func BenchmarkStringRpcCall(b *testing.B) {
  client, err := rpc.Dial("tcp", "localhost:1234")
  if err != nil {
    log.Fatal("dialing:", err)
  }

  args := "hello world"
  var reply string

  for i := 0; i < b.N; i++ {
    err = client.Call("Gobby.ToUpper", args, &reply)
    if err != nil {
      log.Fatal("gobby error:", err)
    }
  }
}

// RAW : takes 57,706 ns/op
func BenchmarkRawwGo_refl_RcpCall(b *testing.B) {
  conn, err := net.Dial("tcp", "localhost:1235")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 7
  var y int64 = 8
  var z int64

  for i := 0; i < b.N; i++ {
    binary.Write(conn, binary.BigEndian, &x)
    binary.Write(conn, binary.BigEndian, &y)
    binary.Read(conn, binary.BigEndian, &z)
  }
  conn.Close()
}

// RAW : takes 14,328 ns/op
func BenchmarkRawwGo_hand_RcpCall(b *testing.B) {
  conn, err := net.Dial("tcp", "localhost:1236")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 7
  var y int64 = 8
  var z int64

  for i := 0; i < b.N; i++ {
    binary.Write(conn, binary.BigEndian, &x)
    binary.Write(conn, binary.BigEndian, &y)
    binary.Read(conn, binary.BigEndian, &z)
  }
  conn.Close()
}

// RAW : takes 12,599 ns/op
func BenchmarkRawwCRcpCall(b *testing.B) {
  conn, err := net.Dial("tcp", "localhost:1236")
  if err != nil {
    log.Fatal("dialing:", err)
  }
  var x int64 = 7
  var y int64 = 8
  var z int64

  for i := 0; i < b.N; i++ {
    binary.Write(conn, binary.BigEndian, &x)
    binary.Write(conn, binary.BigEndian, &y)
    binary.Read(conn, binary.BigEndian, &z)
  }
  conn.Close()
}

