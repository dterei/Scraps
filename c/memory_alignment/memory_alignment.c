#include "memory_alignment.h"

int AREA[100];
long long PTR = (long long) &AREA;

char read_char_aligned(void)
{
	char *p = (char*) PTR;
	return *p;
}

void store_char_aligned(char c)
{
	char *p = (char*) PTR;
	*p = c;	
}

char read_char_unaligned(void)
{
	char *p = (char*) (PTR + 1);
	return *p;
}

void store_char_unaligned(char c)
{
	char *p = (char*) (PTR + 1);
	*p = c;	
}

int read_int_aligned(void)
{
	int *p = (int*) PTR;
	return *p;
}

void store_int_aligned(int i)
{
	int *p = (int*) PTR;
	*p = i;
}

int read_int_unaligned(void)
{
	int *p = (int*) (PTR + 1);
	return *p;
}

void store_int_unaligned(int i)
{
	int *p = (int*) (PTR + 1);
	*p = i;
}

