CFLAGS=-O0 -g -DDEBUG
LDLIBS=-lrt

EXECUTABLES=shm_client shm_server shm_mmap_client shm_mmap_server \
	shm_posix_client shm_posix_server shm_allocator tls tls_glibc tls_set_thread

all: $(EXECUTABLES)

tls_glibc: tls_glibc.c
	$(CC) $(CFLAGS) -static -pthread -o tls_glibc tls_glibc.c

tls: tls.c
	$(CC) $(CFLAGS) -static -pthread -o tls tls.c

tls_set_thread: tls_set_thread.c
	$(CC) $(CFLAGS) -static -pthread -o tls_set_thread tls_set_thread.c

.PHONY: clean
clean:
	@rm $(EXECUTABLES)
