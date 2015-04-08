/* TSC Handling  */
#ifndef __TSC_HDR
#define __TSC_HDR

#define CPU_FREQ 2401000000
#define NS_PER_CYCLE 0.416493128
#define N	10000

static inline unsigned long cycles_to_ns(unsigned long cycles)
{
  return cycles * NS_PER_CYCLE;
}

static inline void sync_tsc(void)
{
	asm volatile("cpuid" : : : "%rax", "%rbx", "%rcx", "%rdx");
}

static inline unsigned long rdtsc(void)
{
	unsigned int a, d;
	asm volatile("rdtsc" : "=a" (a), "=d" (d) : : "%rbx", "%rcx");
	return ((unsigned long) a) | (((unsigned long) d) << 32);
}

static inline unsigned long rdtscp(void)
{
	unsigned int a, d;
	asm volatile("rdtscp" : "=a" (a), "=d" (d) : : "%rbx", "%rcx");
	return ((unsigned long) a) | (((unsigned long) d) << 32);
}

static unsigned long measure_tsc_overhead(void)
{
	unsigned long t0, t1, overhead = ~0UL;
	int i;

	for (i = 0; i < N; i++) {
		t0 = rdtsc();
		asm volatile("");
		t1 = rdtscp();
		if (t1 - t0 < overhead)
			overhead = t1 - t0;
	}

	return overhead;
}

static inline unsigned long bench_start(void)
{
  sync_tsc();
  return rdtsc();
}

static inline unsigned long bench_end(void)
{
  unsigned long t = rdtscp();
  sync_tsc();
  return t;
}
#endif
