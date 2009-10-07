#include <stdio.h>

int __attribute__((__stdcall__)) f(void)
{
	printf("Hello from f!\n");
}

int main(void)
{
	f();
	return 0;
}

