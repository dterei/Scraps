hello: hello.go
	CGO_ENABLED=0 go build -tags netgo hello.go

hello-0.0.1-linux-amd64.aci:
	./build-aci.sh

aci: hello-0.0.1-linux-amd64.aci

.PHONY: clean
clean:
	rm -f hello
	rm -f hello-0.0.1-linux-amd64.aci
