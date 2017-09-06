.PHONY: all
all: recv sender

recv: recv.go
	go build -o recv ./recv.go

sender: sender.go
	go build -o sender ./sender.go

.PHONY: clean
clean:
	rm -f recv sender
