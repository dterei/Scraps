#include <stdio.h>

int run(char);

int main(void)
{
	char i = getchar();
	return run(i);
}

int run(char i)
{
	if (i > 0) {
		int *p = (int*) 0;
		int v = *p;
		printf("Null pointer: %d\n", v);
		return 0;
	} else {
		printf("i is %d\n", i);
		return run(i+1);
	}
}

