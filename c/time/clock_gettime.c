#include "utils.h"

/*
 * Clock types:
 * CLOCK_REALTIME
 * CLOCK_MONOTONIC
 * CLOCK_PROCESS_CPUTIME_ID
 * CLOCK_THREAD_CPUTIME_ID
 */

/* #define CLOCK CLOCK_REALTIME */
/* #define CLOCK CLOCK_REALTIME_COARSE */
/* #define CLOCK CLOCK_MONOTONIC */
#define CLOCK CLOCK_MONOTONIC_COARSE
/* #define CLOCK CLOCK_PROCESS_CPUTIME_ID */
/* #define CLOCK CLOCK_THREAD_CPUTIME_ID */

int main(void)
{
	struct timespec t;
	int r;
	long i;

	// sanity check
	r = clock_gettime(CLOCK, &t);
	if (r < 0) {
		printf("error with clock_gettime! (%i)\n", r);
		exit(1);
	} else {
		/* printf("time is: %s", ctime(&t.tv_sec)); */
		/* printf("time as int is: %lins\n", t.tv_sec * SECONDS + t.tv_nsec * NANOSECONDS); */
	}

	test_start();
	for (i = 0; i < N; i++) {
		clock_gettime(CLOCK, &t);
	}
	test_end();

	return 0;
}

