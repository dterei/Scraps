// Demonstration of low-level C benchmarking.
//

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <sys/syscall.h>
#include <sys/time.h>

#include "tsc.h"

#define NS 1
#define US 1000
#define MS 1000000
#define SS 1000000000

#define CLOCK CLOCK_MONOTONIC

#define N 10000

uint64_t tsc_overhead;

// get the current time in nanoseconds
inline uint64_t clock_ns()
{
  struct timespec t;
  clock_gettime(CLOCK, &t);
  return (uint64_t) t.tv_sec * SS + t.tv_nsec * NS;
}

// measure the cost to call `clock_gettime` for the specified clock
uint64_t clock_overhead()
{
  int i;
  struct timespec t;
  uint64_t  t0, t1, overhead = ~0;

  // we run N times and take the min
  for (i = 0; i < N; i++) {
    t0 = bench_start();
    clock_gettime(CLOCK, &t);
    t1 = bench_end();
    if (t1 - t0 < overhead)
      overhead = t1 - t0;
  }

  return overhead - tsc_overhead;
}

// recursive fibonacci calculation
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

// evaluate cost of recursive fibonacci calculation
void bench_fib()
{
  uint64_t t0, t1;
  int i;

  t0 = bench_start();
  for (i = 0; i < N; i++) {
    fib(10);
  }
  t1 = bench_start();

  printf("Fib (10): %" PRIu64 " clock cycles\n",
    (t1 - t0 - tsc_overhead) / N);
}

// evaluate cost of a simple as possible system call
void bench_syscall(void)
{
  uint64_t t0, t1;
  int i;

  t0 = bench_start();
  for (i = 0; i < N; i++) {
    syscall(SYS_getpid);
  }
  t1 = bench_end();

  printf("System call (getpid): %" PRIu64 " cycles\n",
    (t1 - t0 - tsc_overhead) / N);
}

// compare using tsc and `clock_gettime` for benchmarking code.
void bench_tsc_clock(unsigned int fibn)
{
  printf("=> TSC Vs. Clock accuray (need to adjust TSC freq to machine)\n");
  printf("Testing `fib(%d)`\n", fibn);

  uint64_t t0, t1, coverhead;
  int i;

  t0 = bench_start();
  for (i = 0; i < N; i++) {
    fib(fibn);
  }
  t1 = bench_start();
  printf("TSC  : %" PRIu64 " cycles\n", (t1 - t0 - tsc_overhead) / N);
  printf("TSC  : %" PRIu64 " ns\n", cycles_to_ns((t1 - t0 - tsc_overhead) / N));

  t0 = clock_ns();
  for (i = 0; i < N; i++) {
    fib(fibn);
  }
  t1 = clock_ns();
  coverhead = clock_overhead(CLOCK);
  printf("CLOCK: %" PRIu64 " ns\n", (t1 - t0 - coverhead) / N);

  printf("\n=> TSC Vs. Clock call-cost\n");
  printf("TSC  : %" PRIu64 " cycles\n", tsc_overhead);
  printf("CLOCK: %" PRIu64 " cycles\n\n", coverhead);
}

int main(void)
{
  tsc_overhead = measure_tsc_overhead();

  bench_tsc_clock(5);
  bench_tsc_clock(10);
  bench_tsc_clock(25);

  printf("=> Bench code\n");
  bench_syscall();
  bench_fib();

  return 0;
}

