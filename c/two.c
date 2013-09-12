// from http://blog.regehr.org/archives/1033
// demonstrate that this will be all optimized to just:
//
// int main (void)
// {
//   printf ("checked 4294967296 values\n");
//   return 0;
// }
//

#include <stdio.h>
#include <limits.h>
#include <assert.h>
 
void foo (unsigned x)
{
  unsigned x1 = ~x;
  unsigned x2 = -x - 1;
  assert (x1 == x2);
}
 
int main (void)
{
  unsigned x = 0;
  long checked = 0;
  while (1) {
    foo (x);
    checked++;
    if (x==UINT_MAX) break;
    x++;
  }
  printf ("checked %ld values\n", checked);
  return 0;
}

