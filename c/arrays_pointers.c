#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	char b[] = {'x', 'y', 'z'};
	char array[] = {'a', 'b', 'c', 'd', 'e', 'f', 'h'};
	char *pointer = array;

	printf("array: p = %p, d = %d, size = %d\n", array, array, sizeof(array));
	printf("pointer: p = %p, d = %d, size = %d\n", pointer, pointer, sizeof(pointer));

	printf("array: size = %d\n", array_size(array));
	printf("array: size2 = %d\n", array_size2(array));
	printf("array: size = %d\n", array_size(pointer));
	printf("array: size2 = %d\n", array_size2(pointer));

	printf("array[1] = %c\n", array[1]);
	printf("pointer+1 = %c\n", pointer[1]);

	array[1] = 'e';

	printf("array[1] = %c\n", array[1]);
	printf("pointer+1 = %c\n", pointer[1]);

	return 0;
}

int array_size(char *a)
{
	return sizeof(a);
}

int array_size2(char a[])
{
	return sizeof(a);
}

