#include "tbb/parallel_sort.h"
#include <math.h>

using namespace tbb;

const int N = 100000;
float a[N], b[N], c[N], d[N];

void SortExample() {
  for( int i = 0; i < N; i++ ) {
     a[i] = sin((double)i);
     b[i] = cos((double)i);
     c[i] = 1/sin((double)i);
     d[i] = 1/cos((double)i);
  }
  parallel_sort(a, a + N);
  parallel_sort(b, b + N, std::greater<float>());
  parallel_sort(c);
  parallel_sort(d, std::greater<float>());
}

int main( void )
{
  SortExample();
  return 0;
}

