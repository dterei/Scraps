.PHONY: all
all: runner p1 p2

.PHONY: runner
runner:
	go build -o main ./runner

.PHONY: p1
p1:
	go build -buildmode=plugin ./p1

.PHONY: p2
p2:
	go build -buildmode=plugin ./p2

.PHONY: view-p1
view-p1:
	file ./p1.so
	ldd ./p1.so

.PHONY: clean
clean:
	-rm p1.so
	-rm p2.so
	-rm main

.PHONY: dev
dev:
	@./main -plugin1 ./p1.so -plugin2 ./p2.so
