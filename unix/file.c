#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
  printf("O_RDWR: %x\n", O_RDWR);
  printf("O_CREAT: %x\n", O_CREAT);
  return 0;
}
