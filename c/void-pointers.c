#include <stdio.h>
#include <stdlib.h>

#define TP(n, x, v) printf("Method %d: %s - ", (n++), #x);x;printf("%x\n", (v));free(v)

int main(void)
{
	printf("Testing different ways of using malloc...\n");
	int count = 1;

	TP(count, void *vp = malloc(sizeof(int) * 10), vp);
	TP(count, long *lp = (long*) malloc(sizeof(long) * 10), lp);
	TP(count, int *ip = malloc(sizeof(int) * 10), ip);
	TP(count, char *ip2 = malloc(sizeof(*ip2) * 10), ip2);

	printf("Done!\n");

	return 0;
}

