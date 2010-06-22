#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	float f = 1.3;
	double d = 1.33333333333333;
	float fnan = 1 / 0;
	double dnan = 1 / 0;
	printf("f = %f, d = %f\n", f, d);
	printf("fnan = %f, dnan = %f\n", fnan, dnan);
	return 0;
}

