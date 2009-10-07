#include <stdio.h>

extern int Mp[];

int main(void)
{
	//1: int z = (int) Mp;
	//2: int z = (int) &Mp;
	int a = Mp[0];
	int b = Mp[1];
	int c = Mp[2];
	int d = (int) Mp;
	int e = (int) (&Mp);

	printf("a = %d\nb = %d\nc = %d\n", a, b, c);
	printf("d = %d\ne = %d\n", d, e);

	return 0;
}

