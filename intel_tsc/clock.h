// Utilities for benchmarking different `clock_gettime` clocks.
//

#ifndef __CLOCK_HDR
#define __CLOCK_HDR

#include <inttypes.h>
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

/* get the current time in nanoseconds. */
static inline uint64_t clock_ns(clockid_t clock)
{
	struct timespec t;
  clock_gettime(clock, &t);
	return (uint64_t) t.tv_sec * SS + t.tv_nsec * NS;
}

/* measure the cost to call `clock_gettime` for the specified clock. */
static inline uint64_t clock_overhead(clockid_t clock)
{
	int i;
	struct timespec t;
	uint64_t  t0, t1, overhead = ~0, tsc_overhead;

  tsc_overhead = measure_tsc_overhead();

  // we run N times and take the min
	for (i = 0; i < N; i++) {
		t0 = rdtsc();
		clock_gettime(clock, &t);
		t1 = rdtscp();
		if (t1 - t0 < overhead)
			overhead = t1 - t0;
	}

  return overhead - tsc_overhead;
}

/* measure + print the cost to call `clock_gettime` for the specified clock. */
static inline measure_clock(clockid_t clock)
{
	int i;
	struct timespec t;
	uint64_t t0, t1, overhead;

  clock_getres(clock, &t);
  printf("clock_getres: %ld seconds | %ld nanoseconds\n", t.tv_sec, t.tv_nsec);

  overhead = clock_overhead(clock);
	printf("clock_gettime: " PRIu64 " cycles\n", overhead);
}

#endif
