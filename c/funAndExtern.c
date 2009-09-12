#include <stdio.h>

extern void fun(void);

int main(void)
{
	fun();
	return 0;
}

void fun(void)
{
	printf("Hello from fun!");
}

