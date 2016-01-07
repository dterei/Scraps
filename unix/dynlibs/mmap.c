#include <stdio.h>
#include <sys/mman.h>

int main(void)
{
  void * ptr = mmap(NULL, 1024*1024, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if (ptr == MAP_FAILED) {
    perror("mmap failed:");
    return 1;
  }
  printf("%p\n", ptr);
  return 0;
}
