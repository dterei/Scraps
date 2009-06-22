#include <stdio.h>
#include <stdlib.h>

int gvar = 1;

int main(void)
{
	int *localvar = &gvar;
	int *t = (int*) malloc(sizeof(int) * 10);
	printf("Hello World\n");
	asm("/* GCROOT %0 */" : "=g"(localvar) : "0"(localvar) : "memory");
	asm("/* GCROOT %0 */" : "=g"(t) : "0"(t) : "memory");
	free(t);
	return 0;
}

