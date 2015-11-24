#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

// An assert that ideally is compile time... doesn't work that well.

void _dynamic_assert(bool test, char * msg)
  __attribute__((warning("dynamic assert couldn't be statically removed!")));

#define dynamic_assert(exp, msg)    \
  (!__builtin_constant_p (exp)  \
  ? _dynamic_assert(exp, msg) : _Static_assert(exp, msg))

void _dynamic_assert(bool test, char * msg)
{
  if (!test) {
    fprintf(stderr, "Assert failed: %s\n", msg);
    assert(test);
  }
}

int main(int argc, char *argv[])
{
  dynamic_assert(true, "check true");
  int a = getc(stdin);
  dynamic_assert(a);
  return 0;
}
