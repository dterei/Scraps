#include <stdio.h>

void fun(int a)
{
	if (1 == a) {
		return;
	}
	printf("Hello World!\n");
	return;
}

int main(void)
{
	fun(2);
	return 0;
}

