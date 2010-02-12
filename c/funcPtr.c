#include <stdio.h>

int show(void)
{
	printf("Line 1\n");
	printf("Line 2\n");
	printf("Line 3\n");
	printf("Line 4\n");
	printf("Line 5\n");
	return 0;
}

int main(void)
{
	int (*fnPtr) (void) = show;
	(*fnPtr) ();
	return 0;
}

