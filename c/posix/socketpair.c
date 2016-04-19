#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

// - Test socketpair -
// Both sockets are bi-directional and buffered (can write and read back from
// socket in one thread).

void parent(int fdin, int fdout)
{
  size_t n;
  char buf[256];
	printf("[parent] I'm the parent!\n");
  printf("[parent] Socket FD: %d, %d\n", fdin, fdout);

  // write fdout, read fdin
  write(fdout, "Hello from parent!", 19);
  n = read(fdin, buf, 256);
  printf("[parent] Got message: %s\n", buf);

  // reverse: write fdin, read fdout
  write(fdin, "Hello from parent!", 19);
  n = read(fdout, buf, 256);
  printf("[parent] Got message: %s\n", buf);
}

void child(int fdin, int fdout)
{
  size_t n;
  char buf[256];
	printf("[child] I'm the child!\n");
  printf("[child] Socket FD: %d, %d\n", fdin, fdout);

  // write fdout, read fdin
  write(fdout, "Hello from child!", 18);
  n = read(fdin, buf, 256);
  printf("[child] Got message: %s\n", buf);

  // reverse: write fdin, read fdout
  write(fdin, "Hello from child!", 19);
  n = read(fdout, buf, 256);
  printf("[child] Got message: %s\n", buf);
}

int main(void)
{
  int fds[2];
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds)) {
    perror("socketpair");
  }

	if (fork()) {
		parent(fds[0], fds[1]); // parent = !0 (pid of child)
	} else {
		child(fds[1], fds[0]);  // child = 0
	}

	return 0;
}
