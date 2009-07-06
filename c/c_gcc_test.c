#include <stdio.h>

int c;
__thread int d;
register int R1 __asm__ ("%" "i5");

int main(void)
{
	c = 3;
	R1 = 5;
	d = 4;
	printf("C: %d\n", c);
	printf("D: %d\n", d);
	printf("R1: %d\n", R1);
	R1 *= 2;
	printf("R1: %d\n", R1);
	return 0;
}

