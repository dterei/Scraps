#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>

#define N 100000000

#define SECONDS      1000000000
#define MILLISECONDS 1000000
#define MICROSECONDS 1000
#define NANOSECONDS  1

unsigned long long t_start;

unsigned long long gettime(void)
{
	struct timespec t;
	int r;

	r = clock_gettime(CLOCK_MONOTONIC, &t);
	if (r < 0) {
		printf("error with gettimeofday! (%i)\n", r);
		exit(1);
	}

	return (unsigned long long) t.tv_sec * SECONDS + t.tv_nsec * NANOSECONDS;
}

void test_start(void)
{
	t_start = gettime();
}

void test_end(void)
{
	unsigned long long t_end, t_per;
	t_end = gettime();
	t_per = (t_end - t_start) / N;
	printf("time per call: %lluns\n", t_per);
}

