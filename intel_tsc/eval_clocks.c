// Evaluate cost of calling different clocks under Linux. On one machine at a
// point in time, we got these values:
//
// time                     :   3 cycles
// ftime                    :  54 cycles
// gettimeofday             :  42 cycles
//
// CLOCK_MONOTONIC_COARSE   :   9 cycles
// CLOCK_REALTIME_COARSE    :   9 cycles
// CLOCK_MONOTONIC          :  42 cycles
// CLOCK_REALTIME           :  42 cycles
// CLOCK_MONOTONIC_RAW      : 173 cycles
// CLOCK_BOOTTIME           : 179 cycles
// CLOCK_THREAD_CPUTIME_ID  : 349 cycles
// CLOCK_PROCESS_CPUTIME_ID : 370 cycles
//
// Numbers above generated on Intel Core i7-4771 CPU @ 3.50GHz on Linux 4.0.
//

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <sys/timeb.h>
#include <time.h>

#include "tsc.h"

#define N	100000

/* measure the cost to call `time`. */
void time_overhead()
{
	int i;
	uint64_t t0, t1, tsc_overhead;
  uint64_t min, max, avg;
  uint64_t times[N];

  tsc_overhead = measure_tsc_overhead();

  // we run N times and take the min
	for (i = 0; i < N; i++) {
		t0 = bench_start();
		time(NULL);
		t1 = bench_end();
    times[i] = t1 - t0 - tsc_overhead;
	}
  
  min = ~0, max = 0, avg = 0;
  for (i = 0; i < N; i++) {
    avg += times[i];
    if (times[i] < min) { min = times[i]; }
    if (times[i] > max) { max = times[i]; }
  }
  avg /= N;
  
  printf("\n- TIME -\n");
  printf("time_getres: 1s\n");
	printf("Cost (min): %" PRIu64 " cycles\n", min);
  printf("Cost (avg): %" PRIu64 " cycles\n", avg);
	printf("Cost (max): %" PRIu64 " cycles\n", max);
}

/* measure the cost to call `ftime`. */
void ftime_overhead()
{
	int i;
	struct timeb t;
	uint64_t t0, t1, tsc_overhead;
  uint64_t min, max, avg;
  uint64_t times[N];

  tsc_overhead = measure_tsc_overhead();

  // we run N times and take the min
	for (i = 0; i < N; i++) {
		t0 = bench_start();
		ftime(&t);
		t1 = bench_end();
    times[i] = t1 - t0 - tsc_overhead;
	}
  
  min = ~0, max = 0, avg = 0;
  for (i = 0; i < N; i++) {
    avg += times[i];
    if (times[i] < min) { min = times[i]; }
    if (times[i] > max) { max = times[i]; }
  }
  avg /= N;
  
  printf("\n- FTIME -\n");
  printf("ftime_getres: 1ms\n");
	printf("Cost (min): %" PRIu64 " cycles\n", min);
  printf("Cost (avg): %" PRIu64 " cycles\n", avg);
	printf("Cost (max): %" PRIu64 " cycles\n", max);
}

/* measure the cost to call `gettimeofday`. */
void gettimeofday_overhead()
{
	int i;
	struct timeval t;
	uint64_t t0, t1, tsc_overhead;
  uint64_t min, max, avg;
  uint64_t times[N];

  tsc_overhead = measure_tsc_overhead();

  // we run N times and take the min
	for (i = 0; i < N; i++) {
		t0 = bench_start();
		gettimeofday(&t, NULL);
		t1 = bench_end();
    times[i] = t1 - t0 - tsc_overhead;
	}
  
  min = ~0, max = 0, avg = 0;
  for (i = 0; i < N; i++) {
    avg += times[i];
    if (times[i] < min) { min = times[i]; }
    if (times[i] > max) { max = times[i]; }
  }
  avg /= N;
  
  printf("\n- GETTIMEOFDAY -\n");
  printf("gettimeofday_getres: 1us (?)\n");
	printf("Cost (min): %" PRIu64 " cycles\n", min);
  printf("Cost (avg): %" PRIu64 " cycles\n", avg);
	printf("Cost (max): %" PRIu64 " cycles\n", max);
}

/* measure the cost to call `clock_gettime` for the specified clock. */
void clock_overhead(clockid_t clock)
{
	int i;
	struct timespec t;
	uint64_t t0, t1, tsc_overhead;
  uint64_t min, max, avg;
  uint64_t times[N];

  tsc_overhead = measure_tsc_overhead();

  // we run N times and take the min
	for (i = 0; i < N; i++) {
		t0 = bench_start();
		clock_gettime(clock, &t);
		t1 = bench_end();
    times[i] = t1 - t0 - tsc_overhead;
	}

  
  min = ~0, max = 0, avg = 0;
  for (i = 0; i < N; i++) {
    avg += times[i];
    if (times[i] < min) { min = times[i]; }
    if (times[i] > max) { max = times[i]; }
  }
  avg /= N;
  
	printf("Cost (min): %" PRIu64 " cycles\n", min);
  printf("Cost (avg): %" PRIu64 " cycles\n", avg);
	printf("Cost (max): %" PRIu64 " cycles\n", max);
}

/* measure + print the cost to call `clock_gettime` for the specified clock. */
void measure_clock(const char * ctype, clockid_t clock)
{
	struct timespec t;

  printf("\n- %s -\n", ctype);

  clock_getres(clock, &t);
  printf("clock_getres: %ld seconds | %ld nanoseconds\n", t.tv_sec, t.tv_nsec);

  clock_overhead(clock);
}

#define eval_clock(clk) \
  measure_clock(#clk, clk)

/* benchmark various clock sources */
int main(void)
{
	printf("=> Testing Clock...\n");
  time_overhead();
  ftime_overhead();
  gettimeofday_overhead();
  eval_clock(CLOCK_REALTIME);
  eval_clock(CLOCK_REALTIME_COARSE);
  eval_clock(CLOCK_MONOTONIC);
  eval_clock(CLOCK_MONOTONIC_COARSE);
  eval_clock(CLOCK_MONOTONIC_RAW);
  eval_clock(CLOCK_BOOTTIME);
  eval_clock(CLOCK_PROCESS_CPUTIME_ID);
  eval_clock(CLOCK_THREAD_CPUTIME_ID);

  return 0;
}

