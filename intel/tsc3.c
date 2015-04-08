#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "clock.h"

unsigned long overhead;

void inline load(volatile int *var)
{
  (*var) = 1;
}

unsigned int fib(unsigned int n)
{
  if (n == 0) {
    return 0;
  } else if (n == 1) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

void bench_fib()
{
  unsigned long t0, t1;
  int i;

  t0 = bench_start();
  for (i = 0; i < N; i++) {
    fib(10);
  }
  t1 = bench_start();

  printf("Fib: %ld clock cycles\n", (t1 - t0 - overhead) / N);
}

void bench_load()
{
  unsigned long t0, t1;
  int i, variable = 0;

  t0 = bench_start();
	for (i = 0; i < N; i++) {
    load(&variable);
  }
  t1 = bench_end();

  printf("Load: %" PRIu64 " clock cycles\n", (t1 - t0 - overhead) / N);
}

static void bench_syscall(void)
{
	int i;
	unsigned long t0, t1;

  t0 = bench_start();
	for (i = 0; i < N; i++) {
		syscall(SYS_gettid);
	}
  t1 = bench_end();

	printf("System call: %ld cycles\n", (t1 - t0 - overhead) / N);
}

static void bench_clock_gettime(void)
{
	printf("\n=> Testing Clock...\n");

  printf("- CLOCK_REALTIME -\n");
  measure_clock(CLOCK_REALTIME);

  printf("- CLOCK_REALTIME_COARSE -\n");
  measure_clock(CLOCK_REALTIME_COARSE);

  printf("- CLOCK_MONOTONIC -\n");
  measure_clock(CLOCK_MONOTONIC);

  printf("- CLOCK_MONOTONIC_COARSE -\n");
  measure_clock(CLOCK_MONOTONIC_COARSE);

  printf("- CLOCK_MONOTONIC_RAW -\n");
  measure_clock(CLOCK_MONOTONIC_RAW);

  printf("- CLOCK_BOOTTIME -\n");
  measure_clock(CLOCK_BOOTTIME);

  printf("- CLOCK_PROCESS_CPUTIME_ID -\n");
  measure_clock(CLOCK_PROCESS_CPUTIME_ID);

  printf("- CLOCK_THREAD_CPUTIME_ID -\n");
  measure_clock(CLOCK_THREAD_CPUTIME_ID);
}

void bench_tsc_clock()
{
	printf("\n=> TSC Vs. Clock...\n");

  printf("Iterations: %d\n", N);

  unsigned long t0, t1, coverhead;
  int i;

  t0 = bench_start();
  for (i = 0; i < N; i++) {
    fib(5);
  }
  t1 = bench_start();

  printf("TSC: %ld clock cycles\n", (t1 - t0 - overhead) / N);
  printf("TSC: %ld ns\n", cycles_to_ns((t1 - t0 - overhead) / N));

  t0 = clock_ns();
  for (i = 0; i < N; i++) {
    fib(5);
  }
  t1 = clock_ns();
  
  coverhead = clock_overhead(CLOCK);

  printf("CLOCK: %ld ns\n", (t1 - t0 - coverhead) / N);
}

int main(void)
{
	printf("=> Testing TSC...\n");

	overhead = measure_tsc_overhead();
	printf("TSC overhead: %ld cycles\n", overhead);

  bench_syscall();
  bench_load();
  bench_fib();
  bench_clock_gettime();
  bench_tsc_clock();

  return 0;
}

