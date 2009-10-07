#include <stdio.h>

int main(void)
{
	unsigned int x = 4;
	unsigned int y = 2;
	unsigned int z = 2;

	unsigned int a = (x > y) << z;
	printf("a = %d\n", a);
	return 0;
}

