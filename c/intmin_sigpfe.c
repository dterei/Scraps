#include <stdio.h>
#include <limits.h>

// raises a SIGPFE on machines since INT_MIN / -1 can't be represented by
// two's-complement ( INT_MIN = -INT_MAX - 1). But strange it doesn't just
// truncate (but overflow undefined with signed integers).
int main(void)
{
  int i = INT_MIN;
  int j = i / -1;
  printf("INT_MIN (%d) / -1 = %d\n", i, j);
  return j;
}

