#define _GNU_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <dlfcn.h>

pid_t getpid(void)
{
  pid_t (*orig_getpid)(void) = dlsym(RTLD_NEXT, "getpid");
  printf("Calling GETPID\n");
  return orig_getpid();
}
