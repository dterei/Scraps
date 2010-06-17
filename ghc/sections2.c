#include <stdio.h>

// #define TOS(x) ".text,\"ax\",@progbits\n.subsection "#x" #"
#define TOS(x) ".text;.text "#x" #"

#define INF_A TOS(100)
#define FUN_A TOS(8192)

#define INF_B TOS(8193)
#define FUN_B TOS(1111118196)

typedef struct info_table {
	const int srt;
	const int type;
	const int args;
} info_table;

const info_table a_info_tb __attribute__ ((section (INF_A))) = {-1, 1, 231};

int a_entry() __attribute__ ((section (FUN_A)));

int b_entry() __attribute__ ((section (FUN_B)));

int a_entry()
{
	int *p = (int*) (&a_entry) - 1;
	return *p;
}

const info_table b_info_tb __attribute__ ((section (INF_B))) = {-2, 0, 952};

int main(void)
{
	int a = a_entry();
	int ai = a_info_tb.args;
	printf("a (%d) = %d\n", ai, a);
	int b = b_entry();
	int bi = b_info_tb.args;
	printf("b (%d) = %d\n", bi, b);
	return a + b;
}

int b_entry()
{
	int *p = (int*) (&b_entry) - 1;
	return *p;
}

