all: client server

client: Client.go Types.go
	go build Client.go Types.go

server: Server.go Types.go
	go build Server.go Types.go

bench: client server Client_test.go
	./Server&
	go test -bench ".*" -run "x" Client_test.go Types.go
	killall Server

clean:
	rm Server
	rm Client

