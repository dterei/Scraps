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

int previous(int a[])
{
	return a[-1];
}

int main(void)
{
	int one  = head(my_array);
	int prev = previous(&my_array[1]);
	printf("Head of array => %d\n", one);
	printf("Head of array => %d\n", prev);
	return 0;
}

