#include <stdio.h>

void main(void)
{
	int count = 0;
	int ratio = 5;
	int limit = 40;

	while (count < limit) {
		int rem = count % ratio;
		if (rem) {
			printf("Ratio hit! [%d]\n", count);
		} else {
			printf("No hit! [%d]\n", count);
		}
		count++;
	}
}

