/* clock_gettime support  */
#ifndef __CLOCK_HDR
#define __CLOCK_HDR

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

#include "tsc.h"

#define NS 1
#define US 1000
#define MS 1000000
#define SS 1000000000

// #define CLOCK CLOCK_REALTIME // 68 cycles
/* #define CLOCK CLOCK_REALTIME_COARSE // 32 cycles */
#define CLOCK CLOCK_MONOTONIC // 61 cycles
/* #define CLOCK CLOCK_MONOTONIC_COARSE // 22 cycles */
// #define CLOCK CLOCK_MONOTONIC_RAW // 230 cycles
/* #define CLOCK CLOCK_BOOTTIME // 240 cycles */
/* #define CLOCK CLOCK_PROCESS_CPUTIME_ID // 500 cycles */
// #define CLOCK CLOCK_THREAD_CPUTIME_ID // 380 cycles

static inline unsigned long clock_ns(void)
{
	struct timespec t;
  clock_gettime(CLOCK, &t);
	return (unsigned long) t.tv_sec * SS + t.tv_nsec * NS;
}

static inline unsigned long clock_overhead(clockid_t clock)
{
	int i;
	struct timespec t;
	unsigned long t0, t1, overhead = ~0UL, tsc_overhead;

  tsc_overhead = measure_tsc_overhead();

	for (i = 0; i < N; i++) {
		t0 = rdtsc();
		clock_gettime(clock, &t);
		t1 = rdtscp();
		if (t1 - t0 < overhead)
			overhead = t1 - t0;
	}

  return overhead - tsc_overhead;
}

static measure_clock(clockid_t clock)
{
	int i;
	struct timespec t;
	unsigned long t0, t1, overhead;

  overhead = measure_tsc_overhead();

  clock_getres(clock, &t);
  printf("clock_getres: %ld seconds | %ld nanoseconds\n", t.tv_sec, t.tv_nsec);

  overhead = clock_overhead(clock);

	printf("clock_gettime: %ld cycles\n", overhead);
}
#endif
