#include <stdio.h>

int test_cc(int base, int sp, int hp, int r1) __attribute__((fastcall));

int main(void)
{
	int sum = test_cc(1,2,3,4);
	printf("test_cc: %d\n", sum);
	return 0;
}

int __attribute__((fastcall)) test_cc(int base, int sp, int hp, int r1)
{
	int sum = base + sp + hp + r1;
	printf("Base: %d\n", sum);
	return sum;
}

