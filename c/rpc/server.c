#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

// OSX Specific... no standard interface for byteswapping int64...
#include <libkern/OSByteOrder.h>

#define PORT 1237
#define BUF_SIZE 1024

void runClient(int);

int main(void)
{
	int listenfd, connfd, child, optval;
	struct sockaddr_in addr;

	listenfd = socket(PF_INET, SOCK_STREAM, 0);
	optval = 1;
	setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(int));
	setsockopt(listenfd, SOL_SOCKET, SO_KEEPALIVE, &optval, sizeof(int));
	setsockopt(listenfd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(int));

	memset(&addr, 0, sizeof(struct sockaddr_in));

	addr.sin_family = PF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(PORT);

	bind(listenfd, (struct sockaddr *)(&addr), sizeof(struct sockaddr_in));

	listen(listenfd, SOMAXCONN);

	while (1) {
		connfd = accept(listenfd, NULL, NULL);
		child = fork();
		if (child) {
			close(connfd);
		} else {
			close(listenfd);
			runClient(connfd);
		}
	}
}

void runClient(int connfd)
{
	char buf[BUF_SIZE];
	int64_t x, y, z;

	while (1) {
		read(connfd, buf, 16);
		x = *((int64_t*)&buf);
		y = *(((int64_t*)&buf) + 1);
		x = OSSwapBigToHostInt64(x);
		y = OSSwapBigToHostInt64(y);
		z = x * y;
		z = OSSwapHostToBigInt64(z);
		write(connfd, &z, 8);
	}
	close(connfd);
}

