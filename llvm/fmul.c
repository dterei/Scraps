#include <stdio.h>

float g = 0x3faa9fbeU;
float a = 1.3;
double b = 1.3;

int main(void)
{
	float f = 1.45;
	float g = 2.3;
	float h = f * g;
	printf("%f\n", h);
	return 0;
}

