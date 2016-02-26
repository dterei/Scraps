CFLAGS=-O0
LDLIBS=-lrt

EXECUTABLES=shm_client shm_server shm_mmap_client shm_mmap_server \
	shm_posix_client shm_posix_server shm_allocator tls tls_glibc

all: $(EXECUTABLES)

tls_glibc: tls_glibc.c
	$(CC) $(CFLAGS) -pthread -o tls_glibc tls_glibc.c

.PHONY: clean
clean:
	@rm $(EXECUTABLES)
