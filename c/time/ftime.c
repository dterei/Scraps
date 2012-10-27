#include "utils.h"
#include <sys/timeb.h>

int main(void)
{
	struct timeb t;
	long i;
	unsigned long long v;

	// sanity check
	i = ftime(&t);
	if (i < 0) {
		printf("error with ftime! (%li)\n", i);
		exit(1);
	} else {
		/* v = t.time * 1000 + t.millitm; */
		/* printf("time as int is: %llums\n", v); */
	}

	test_start();
	for (i = 0; i < N; i++) {
		ftime(&t);
	}
	test_end();

	return 0;
}


