#include <stdio.h>

// Entry point.
typedef void (*func_t)(int);

// Info table.
// Aligned to 16 bytes because I use arithmetics on info_t* to get entry point
// address from info table address and code sections are commonly so aligned.
// This is target dependent, but overestimating is safe and alignments of more
// than 16 is rare.
typedef struct
{
    // Example payload.
    int n;
} __attribute((aligned(16))) info_t;


// Some example functions.
__attribute((section(".ghc.f1.0")))
static const info_t f1_i = { 1 };

__attribute((section(".ghc.f1.1"),used))
static void f1(int n)
{
   printf("f1: %d\n", n);
}

__attribute((section(".ghc.f2.0")))
static const info_t f2_i = { 2 };

__attribute((section(".ghc.f2.1"),used))
static void f2(int n)
{
   printf("f2: %d\n", n*2);
}


int main()
{
   // Run through some info pointers and call entry points.
   static const info_t *const infos[] = { &f1_i, &f2_i, 0 };

   int i;
   for (i = 0; infos[i]; i++)
   {
       func_t func = (func_t)(infos[i] + 1);
       const info_t *info = infos[i];
       func(info->n);
   }
   return 0;
}
