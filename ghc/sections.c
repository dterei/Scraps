#include <stdio.h>

typedef struct info_table {
	const int srt;
	const int type;
	const int args;
} info_table;

const info_table a_info_tb __attribute__ ((section (".text 1"))) = {-1, 1, 231};

const info_table b_info_tb __attribute__ ((section (".text 3"))) = {-2, 0, 952};

int a_entry() __attribute__ ((section (".text 2")));

int b_entry() __attribute__ ((section (".text 4")));

int a_entry()
{
	int *p = (int*) (&a_entry) - 1;
	return *p;
}

int b_entry()
{
	int *p = (int*) (&b_entry) - 1;
	return *p;
}

int main(void)
{
	int a = a_entry();
	printf("a = %d\n", a);
	int b = b_entry();
	printf("b = %d\n", b);
	return a + b;
}

