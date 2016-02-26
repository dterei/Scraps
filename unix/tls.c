#define _GNU_SOURCE

#include <asm/prctl.h>
#include <sys/prctl.h>

#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* FS Support */
int arch_prctl(int code, unsigned long addr);

void set_fs(void * addr)
{
  if (arch_prctl(ARCH_SET_FS, (unsigned long) addr)) {
    perror("arch_prctl(ARCH_SET_FS)");
    exit(EXIT_FAILURE);
  }
}

unsigned long get_fs(void)
{
  unsigned long fs_addr;
  if (arch_prctl(ARCH_GET_FS, (unsigned long) &fs_addr)) {
    perror("arch_prctl(ARCH_GET_FS)");
    exit(EXIT_FAILURE);
  }
  return fs_addr;
}

/* Thread structure */
typedef struct {
  int tid;
} thread_state;

#define MAX_THREADS 100

/* All threads */
thread_state all_threads[MAX_THREADS];

/* My current thread */
thread_state *my_thread;

/* Raw threads */
int new_thread(int (*fn)(void*))
{
  #define STACK_SZ (1024*1024)
  void * stack = malloc(STACK_SZ);
  if (stack == NULL) {
    perror("malloc(STACK_SZ)");
    exit(EXIT_FAILURE);
  }
  stack += STACK_SZ;

  /* Clone allows specifying the FS/TLS register value - pthreads does this */
  return clone(fn, stack, CLONE_FILES | CLONE_FS | CLONE_SIGHAND | CLONE_THREAD | CLONE_VM | SIGCHLD, NULL);
}

__thread int thread_i;

void print_id(char *who, int tid, int * addr)
{
  *addr = tid;
  for (int i = 0; i < 5; i++) {
    printf("[%s] thread_i: %i (%p)\n", who, *addr, addr);
    usleep(1000000);
  }
}

int child(void * arg)
{
  printf("[child] started\n");
  print_id("child", 0, &thread_i);
  usleep(500000);
  return 0;
}

int main(void)
{
  int tid, status;

  printf("[main_] started\n");

  tid = new_thread(&child);
  printf("[main_] child launched: %d\n", tid);

  print_id("main_", 1, &thread_i);
  usleep(500000);

  return EXIT_SUCCESS;
}

