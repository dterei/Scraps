#include <stdio.h>

#define INF_LIM 1000000000

int main(void)
{
	double v, i;

	for (v = 0, i = 1; i < INF_LIM; i++) {
		v += 1.0 / i;
	}

	printf("lim (n-> infinity) (1/n) = %f\n", v);
	return 0;
}

