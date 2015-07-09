// Basic RDTSC test to time a load from memory.
//

#include <inttypes.h>
#include <stdio.h>

#include "tsc.h"

// basic load to benchmark
void load(volatile int *var)
{
  asm volatile("");
  (*var) = 1;
}

// The following approach is wrong as RDTSC allows re-ordering of instructions
// around it, so we could have benchmark code move outside our clock region, or
// non-benchmark code move into it.
void rdtsc()
{
  uint64_t start, end;
  uint32_t cycles_low, cycles_high, cycles_low1, cycles_high1;
  int variable = 0;

  asm volatile( "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high), "=r" (cycles_low)
                :: "%rax", "%rdx" );

  load(&variable);

  asm volatile( "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high1), "=r" (cycles_low1)
                :: "%rax", "%rdx" );

  start = ((uint64_t) cycles_high << 32) | cycles_low;
  end = ((uint64_t) cycles_high1 << 32) | cycles_low1;

  if (end < start) {
    printf("RDTSC       : Error! end < start\n");
  } else {
    printf("RDTSC       : Execution took %" PRIu64 " clock cycles\n", end - start);
  }
}

// We first attempt to fix this by using CPUID just before RDTSC, as CPUID is a
// full memory barrier instruction, serializing everything before and after it.
//
// This works but CPUID has lots of variance in cycles associated with it so it
// makes our timings less accurate and unable to measure fine-grained times.
//
// Only really the second CPUID call matters as it's the one inside the clock
// region.
void rdtsc_cpuid()
{
  uint64_t start, end;
  uint32_t cycles_low, cycles_high, cycles_low1, cycles_high1;
  int variable = 0;

  asm volatile( "CPUID\n\t" // serialize
                "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high), "=r" (cycles_low)
                :: "%rax", "%rbx", "%rcx", "%rdx" );

  load(&variable);

  asm volatile( "CPUID\n\t" // serialize -- high variance!
                "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high1), "=r" (cycles_low1)
                :: "%rax", "%rbx", "%rcx", "%rdx" );
  
  start = ((uint64_t) cycles_high << 32) | cycles_low;
  end = ((uint64_t) cycles_high1 << 32) | cycles_low1;

  if (end < start) {
    printf("RDTSC_CPUID : Error! end < start\n");
  } else {
    printf("RDTSC_CPUID : Execution took %" PRIu64 " clock cycles\n", end - start);
  }
}

// We fix the variance problem of CPUID by using a newer Intel instruction,
// RDTSCP, that reads the TSC and CPUID in one instruction. RDTSCP is not a
// full barrier though, it only prevents instructions above it moving past it,
// it doesn't prevent instructions below it moving above it.
void rdtscp()
{
  uint64_t start, end;
  uint32_t cycles_low, cycles_high, cycles_low1, cycles_high1;
  int variable = 0;

  asm volatile( "CPUID\n\t" // serialize
                "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high), "=r" (cycles_low)
                :: "%rax", "%rbx", "%rcx", "%rdx" );

  load(&variable);

  asm volatile( "RDTSCP\n\t" // read clock + serialize
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high1), "=r" (cycles_low1)
                :: "%rax", "%rcx", "%rdx" );

  /* do other things */ // these instructions could move above RDTSCP! ^^^

  start = ((uint64_t) cycles_high << 32) | cycles_low;
  end = ((uint64_t) cycles_high1 << 32) | cycles_low1;

  printf("RDTSCP      : Execution took %" PRIu64 " clock cycles\n", end - start);
}

// We fix this final problem by using CPUID again, but now after RDTSCP, since
// we just need to prevent instructions below moving up.
//
// And since the second use of CPUID is outside the clock region, it's variance
// causes no problems.
void rdtscp_cpuid()
{
  uint64_t start, end;
  uint32_t cycles_low, cycles_high, cycles_low1, cycles_high1;
  int variable = 0;

  asm volatile( "CPUID\n\t" // serialize
                "RDTSC\n\t" // read clock
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high), "=r" (cycles_low)
                :: "%rax", "%rbx", "%rcx", "%rdx" );

  load(&variable);

  asm volatile( "RDTSCP\n\t" // read clock + serialize
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                "CPUID\n\t" // serialze -- but outside clock region!
                : "=r" (cycles_high1), "=r" (cycles_low1)
                :: "%rax", "%rbx", "%rcx", "%rdx" );

  start = ((uint64_t) cycles_high << 32) | cycles_low;
  end = ((uint64_t) cycles_high1 << 32) | cycles_low1;

  printf("RDTSCP_CPUID: Execution took %" PRIu64 " clock cycles\n", end - start);
}

void lib_tsc()
{
  uint64_t start, end, overhead;
  int variable = 0;

  start = bench_start();
  load(&variable);
  end = bench_end();

  printf("LIB_TSC     : Execution took %" PRIu64 " clock cycles\n", end - start);

  overhead = measure_tsc_overhead();

  printf("LIB_TSC     : TSC overhead is %" PRIu64 " clock cycles\n", overhead );
}


int main(void)
{
  rdtscp_cpuid();
  rdtsc();
  rdtsc_cpuid();
  rdtscp();
  lib_tsc();
}

