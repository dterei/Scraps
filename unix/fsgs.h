#include <asm/prctl.h>
#include <sys/prctl.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

/* FS/GS Support */
int arch_prctl(int code, unsigned long addr);

void set_fs(void * addr)
{
  if (arch_prctl(ARCH_SET_FS, (unsigned long) addr)) {
    perror("arch_prctl(ARCH_SET_FS)");
    exit(EXIT_FAILURE);
  }
}

void set_gs(void * addr)
{
  if (arch_prctl(ARCH_SET_GS, (unsigned long) addr)) {
    perror("arch_prctl(ARCH_SET_GS)");
    exit(EXIT_FAILURE);
  }
}

unsigned long get_fs(void)
{
  unsigned long fs_addr;
  if (arch_prctl(ARCH_GET_FS, (unsigned long) &fs_addr)) {
    perror("arch_prctl(ARCH_GET_GS)");
    exit(EXIT_FAILURE);
  }
  return fs_addr;
}

unsigned long get_gs(void)
{
  unsigned long fs_addr;
  if (arch_prctl(ARCH_GET_GS, (unsigned long) &fs_addr)) {
    perror("arch_prctl(ARCH_GET_GS)");
    exit(EXIT_FAILURE);
  }
  return fs_addr;
}


