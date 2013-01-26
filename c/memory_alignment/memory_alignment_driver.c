#include <stdio.h>
#include <stdlib.h>
#include "memory_alignment.h"

int main(void)
{
	char c;
	int i;

	printf("Starting...\n");

	printf("Read: char aligned: ");
	c = read_char_aligned();
	printf("%i\n", c);

	printf("Storing char aligned...\n");
	store_char_aligned(c + 1);
	printf("Read: char aligned: ");
	c = read_char_aligned();
	printf("%i\n", c);

	printf("Read: int aligned: ");
	i = read_int_aligned();
	printf("%i\n", i);

	printf("Storing int aligned...\n");
	store_int_aligned(i + 1);
	printf("Read: int aligned: ");
	i = read_int_aligned();
	printf("%i\n", i);

	printf("Read: char unaligned: ");
	c = read_char_unaligned();
	printf("%i\n", c);

	printf("Storing char unaligned...\n");
	store_char_unaligned(c + 1);
	printf("Read: char unaligned: ");
	c = read_char_unaligned();
	printf("%i\n", c);

	printf("Read: int unaligned: ");
	i = read_int_unaligned();
	printf("%i\n", i);

	printf("Storing int unaligned...\n");
	store_int_unaligned(i + 89);
	printf("Read: int unaligned: ");
	i = read_int_unaligned();
	printf("%i\n", i);

	printf("Read: char unaligned: ");
	c = read_char_unaligned();
	printf("%i\n", c);

	printf("Storing char unaligned...\n");
	store_char_unaligned(c + 1);
	printf("Read: char unaligned: ");
	c = read_char_unaligned();
	printf("%i\n", c);

	return 0;
}

