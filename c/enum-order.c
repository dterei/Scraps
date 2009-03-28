#include <stdio.h>
#include <stdlib.h>

enum {
	A = -99,
	B,
	C,
	D,
};

int
main(int argc, char *argv[])
{
	printf("A: %d, B: %d, C: %d, D: %d\n", A, B, C, D);	
	return EXIT_SUCCESS;
}

