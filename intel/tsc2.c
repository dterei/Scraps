#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

void inline load(volatile int *var)
{
  (*var) = 1;
}

void bench_code()
{
  uint64_t start, end;
  uint32_t cycles_low, cycles_high, cycles_low1, cycles_high1;
  int variable = 0;

  asm volatile( "RDTSC\n\t"
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                : "=r" (cycles_high), "=r" (cycles_low)
                :: "%rax", "%rdx" );

  load(&variable);

  asm volatile( "RDTSC\n\t"
                "mov %%edx, %0\n\t"
                "mov %%eax, %1\n\t"
                "CPUID\n\t"
                : "=r" (cycles_high1), "=r" (cycles_low1)
                :: "%rax", "%rdx" );
  
  start = ( ((uint64_t) cycles_high << 32) | cycles_low );
  end = ( ((uint64_t) cycles_high1 << 32) | cycles_low1 );

  printf("Execution took %" PRIu64 " clock cycles\n", end - start);
}

int main(void)
{
  bench_code();
  return 0;
}

