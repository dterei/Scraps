#include <stdio.h>

#define REG_Base ebx

int main(void)
{
#define ebx "ebx"
	printf("REG_Base: %s\n", REG_Base);
#define ebx ebb
#define ebb 2
	printf("REG_Base: %d\n", REG_Base);
	return 0;
}

