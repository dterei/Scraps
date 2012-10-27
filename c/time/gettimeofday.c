#include "utils.h"

int main(void)
{
	struct timeval t;
	int r;
	long i;

	// sanity check
	r = gettimeofday(&t, NULL);
	if (r < 0) {
		printf("error with gettimeofday! (%i)\n", r);
		exit(1);
	} else {
		/* printf("time is: %s", ctime(&t.tv_sec)); */
		/* printf("time as int is: %lius\n", t.tv_sec * 1000000 + t.tv_usec); */
	}

	test_start();
	for (i = 0; i < N; i++) {
		gettimeofday(&t, NULL);
	}
	test_end();

	return 0;
}

