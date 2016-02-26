/*
 * How does `set_thread_area` and FS interact?
 *
 * Appears that `set_thread_are` is essentially useless on x64 and just around
 * for legacy reasons, use `arch_prctl` exclusively on x64.
 */
#define _GNU_SOURCE

#include <asm/ldt.h>
#include <asm/prctl.h>
#include <linux/unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>

#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "fsgs.h"

int get_thread_area(struct user_desc *u_info)
{
  return syscall(SYS_get_thread_area, u_info);
}

int set_thread_area(struct user_desc *u_info)
{
  return syscall(SYS_set_thread_area, u_info);
}

int prog(void)
{
  unsigned long fs;
  struct user_desc ta;

  fs = get_fs();
  printf("FS: %lu\n", fs);

  ta.entry_number = 0;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);

  ta.entry_number = 1;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);

  ta.entry_number = 2;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);

  ta.entry_number = 3;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);

  ta.entry_number = 3;
  ta.base_addr = 0x10000;
  ta.limit = 100;
  set_thread_area(&ta); 

  memset(&ta, 0, sizeof(struct user_desc));
  ta.entry_number = 3;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);

  ta.entry_number = 2;
  get_thread_area(&ta); 
  printf("TA: entry=%d, base=%lu, length=%d\n", ta.entry_number, ta.base_addr, ta.limit);


  usleep(500000);
}

int main(void)
{
  int status;
  unsigned long glibc_fs = get_fs();
  status = prog();
  set_fs((void *) glibc_fs);
  return status;
}
