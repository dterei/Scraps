#include <stdio.h>

int main(void)
{
	short s = sizeof(short);
	int w1 = sizeof(int);
	int w2 = sizeof(long);
	int w3 = sizeof(long long);
	int p = sizeof(int*);
	int x = sizeof(float);
	int y = sizeof(double);
	printf("short = %d\n", s * 8);
	printf("int = %d\n", w1 * 8);
	printf("long = %d\n", w2 * 8);
	printf("long long = %d\n", w3 * 8);
	printf("pointer = %d\n", p * 8);
	printf("float = %d\n", x * 8);
	printf("double = %d\n", y * 8);
	return 0;
}

