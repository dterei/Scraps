#include <stdio.h>

int my_array[] = {1,2,3,4,5,6};

int hundredth(int a[])
{
	return a[100];
}

int head(int a[])
{
	return a[0];
}

int main(void)
{
	int one = head(my_array);
	printf("Head of array => %d\n", one);
	return 0;
}

