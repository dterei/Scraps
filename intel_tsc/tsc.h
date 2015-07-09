// Support for using the TSC register on intel machines as a timing method.
//

#ifndef __TSC_HDR
#define __TSC_HDR

#include <stdint.h>

#define N	100000

static inline void _sync_tsc(void)
{
	asm volatile("cpuid" : : : "%rax", "%rbx", "%rcx", "%rdx");
}

static inline uint64_t _rdtsc(void)
{
	unsigned a, d;
	asm volatile("rdtsc" : "=a" (a), "=d" (d) : : "%rbx", "%rcx");
	return ((uint64_t) a) | (((uint64_t) d) << 32);
}

static inline uint64_t _rdtscp(void)
{
	unsigned a, d;
	asm volatile("rdtscp" : "=a" (a), "=d" (d) : : "%rbx", "%rcx");
	return ((uint64_t) a) | (((uint64_t) d) << 32);
}

static inline uint64_t bench_start(void)
{
  // unsigned  cycles_low, cycles_high;
  // uint64_t t;
  //
  // asm volatile( "CPUID\n\t" // serialize
  //               "RDTSC\n\t" // read clock
  //               "mov %%edx, %0\n\t"
  //               "mov %%eax, %1\n\t"
  //               : "=r" (cycles_high), "=r" (cycles_low)
  //               :: "%rax", "%rbx", "%rcx", "%rdx" );
  // return ((uint64_t) cycles_high << 32) | cycles_low;

  _sync_tsc();
  return _rdtsc();
}

static inline uint64_t bench_end(void)
{
  // unsigned  cycles_low, cycles_high;
  // uint64_t t;
  //
  // asm volatile( "RDTSCP\n\t" // read clock + serialize
  //               "mov %%edx, %0\n\t"
  //               "mov %%eax, %1\n\t"
  //               "CPUID\n\t" // serialze -- but outside clock region!
  //               : "=r" (cycles_high), "=r" (cycles_low)
  //               :: "%rax", "%rbx", "%rcx", "%rdx" );
  // return ((uint64_t) cycles_high << 32) | cycles_low;

  uint64_t t = _rdtscp();
  _sync_tsc();
  return t;
}

static uint64_t measure_tsc_overhead(void)
{
	uint64_t t0, t1, overhead = ~0;
	int i;

	for (i = 0; i < N; i++) {
		t0 = bench_start();
		asm volatile("");
		t1 = bench_end();
		if (t1 - t0 < overhead)
			overhead = t1 - t0;
	}

	return overhead;
}

// To convert from cycles to wall-clock time we need to know CPU rated
// frequency. Frequency scaling on modern Intel chips doesn't affect the TSC.
#define CPU_FREQ 2401000000
#define NS_PER_CYCLE 0.416493128
static inline uint64_t cycles_to_ns(uint64_t cycles)
{
  return cycles * NS_PER_CYCLE;
}

#endif
