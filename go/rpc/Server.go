package main

import (
  "encoding/binary"
  "log"
  "net"
  "net/rpc"
  "strings"
)

func main() {
  gobby := new(Gobby)
  rpc.Register(gobby)
  l, e := net.Listen("tcp", ":1234")
  if e != nil {
    log.Fatal("listen error:", e)
  }
  go rpc.Accept(l)
  go rawProtocol_refl()
  rawProtocol_hand()
}

type Gobby struct{}

func (t *Gobby) Noop(args *struct{}, reply *struct{}) error {
  return nil
}

func (t *Gobby) Multiply(args *Args, reply *int64) error {
  *reply = args.A * args.B
  return nil
}

func (t *Gobby) ToUpper(str string, reply *string) error {
  *reply = strings.ToUpper(str)
  return nil
}

func rawProtocol_refl() {
  addr, _ := net.ResolveTCPAddr("tcp", ":1235")
  l, e := net.ListenTCP("tcp", addr)
  if e != nil {
    log.Fatal("listen error:", e)
  }

  for {
    conn, _ := l.AcceptTCP()
    conn.SetNoDelay(true);
    conn.SetKeepAlive(true);
    go handleClient(conn)
  }
}

func handleClient(c net.Conn) {
  args := &Args{}
  var z int64 = 0
  for {
    err := binary.Read(c, binary.BigEndian, args)
    if err != nil {
      return
    }
    z = args.A * args.B
    binary.Write(c, binary.BigEndian, &z)
  }
}

func rawProtocol_hand() {
  addr, _ := net.ResolveTCPAddr("tcp", ":1236")
  l, e := net.ListenTCP("tcp", addr)
  if e != nil {
    log.Fatal("listen error:", e)
  }

  for {
    conn, _ := l.AcceptTCP()
    conn.SetNoDelay(true);
    conn.SetKeepAlive(true);
    go handleClient2(conn)
  }
}

func handleClient2(c net.Conn) {
  buf := make([]byte, 16)
  var x, y, z uint64
  var err error

  for {
    _, err  = c.Read(buf)
    if err != nil {
      return
    }
    // cheating a little as treating as uint64, instead of int64...
    // but no impact on speed, and so cant be fucked to figure out code.
    x = binary.BigEndian.Uint64(buf[0:8])
    y = binary.BigEndian.Uint64(buf[8:16])
    z = x * y
    binary.BigEndian.PutUint64(buf, z)
    c.Write(buf[0:8])
  }
}

