// C defines false specifically as 0.
// true = !false, so true is any number but 0.
//
// i.e.,
// false = 0
// true  = !false = !0
#include <stdio.h>

void test_int(int x)
{
  if (x) {
    printf("%3d = true\n", x);
  } else {
    printf("%3d = false\n", x);
  }
}

void main(void)
{
  test_int(-2); // true
  test_int(-1); // true
  test_int(0);  // false
  test_int(1);  // true
  test_int(2);  // true
  test_int(3);  // true
}

