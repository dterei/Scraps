#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#pragma weak two_function
int two_function(void)
{
  return 9;
}

int main(void)
{
  printf("%d\n", getpid());
  return 0;
}

