#include <stdio.h>

int main(void)
{
	int x1 = 4294967295;
	unsigned int y1 = 4294967295;

	unsigned int y2 = y1 - 16 - 8;
	unsigned int y3 = y2 ^ y1;

	printf("x1 = %d\n", x1);
	printf("y1 = %u\n", y1);
	printf("y3 = %u\n", y3);

	return 0;
}

