package main

import "encoding/gob"
import "fmt"
import "log"
import "os"

type page []byte

func main() {
	fmt.Println("vim-go")
	file, err := os.OpenFile("out.gob",
													os.O_WRONLY | os.O_CREATE | os.O_TRUNC,
													0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	enc := gob.NewEncoder(file)

	var p1 page = page("hello world how are you?")
	var pgs []page = []page{p1}
	enc.Encode(pgs)
}
