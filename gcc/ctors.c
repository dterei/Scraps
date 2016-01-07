#include <stdio.h>
#include <stdlib.h>

// https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

// priority: smaller runs before larger number with ctors, reverse for dtors.

void foobar_init(void) __attribute__((constructor (101)));
void foobar_fini(void) __attribute__((destructor (101)));

void foobar_init(void)
{
  printf("Start: Good Morning, Foobar!\n");
  return;
}

void foobar_fini(void)
{
  printf("Exit: Good Night, Foobar!\n");
  return;
}

int main(int argc, char *argv[]) {
  printf("I'm Foobar!\n");
  _Exit(EXIT_SUCCESS);
  return 0;
}

