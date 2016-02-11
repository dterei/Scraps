#include <stdio.h>

int my_array[] = {1,2,3,4,5,6};

void f()
{
	printf("called f\n");
}

void (*f_pointer())(void)
{
	return &f;
}

int hundredth(int a[])
{
	return a[100];
}

int head(int a[])
{
	return a[0];
}

int (*f2())(int*)
{
	return &head;
}

int previous(int a[])
{
	return a[-1];
}

int main(void)
{
	int one  = head(my_array);
	int prev = previous(&my_array[1]);
	int two = f2()(my_array);
	f();
	f_pointer()();
	printf("Head of array => %d\n", one);
	printf("Head of array => %d\n", prev);
	printf("Head of array => %d\n", two);

	return 0;
}

