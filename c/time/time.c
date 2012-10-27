#include "utils.h"

int main(void)
{
	time_t t;
	long i;

	// sanity check
	t = time(NULL);
	if (t < 0) {
		printf("error with time! (%li)\n", t);
		exit(1);
	} else {
		/* printf("time is: %s", ctime(&t)); */
		/* printf("time as int is: %lis\n", t); */
	}

	test_start();
	for (i = 0; i < N; i++) {
		t = time(NULL);
	}
	test_end();

	return 0;
}

