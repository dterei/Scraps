/*
 * Playing with setting FS register ourselves for TLS. Must staticaly link as
 * dynamic linking relies on FS being managed by GCC, libc and linker.
 */
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

#include "fsgs.h"

/* Thread structure */
typedef struct {
  int tid;
  int store;
} thread_state;

#define MAX_THREADS 100

/* All threads */
thread_state all_threads[MAX_THREADS];

/* My current thread */
thread_state *my_thread;

/* Raw threads */
int new_thread(int (*fn)(void*), void *arg)
{
  #define STACK_SZ (1024*1024)
  void * stack = malloc(STACK_SZ);
  if (stack == NULL) {
    perror("malloc(STACK_SZ)");
    exit(EXIT_FAILURE);
  }
  stack += STACK_SZ;

  /* Clone allows specifying the FS/TLS register value - pthreads does this */
  return clone(fn,
    stack,
    CLONE_FILES | CLONE_FS | CLONE_SIGHAND | CLONE_THREAD | CLONE_VM | SIGCHLD,
    arg);
}

/* global counter for thread ID's */
int next_thread_id = 0;

void setup_tls(int tid)
{
  set_fs(all_threads + tid);
  all_threads[tid].tid = tid;
  all_threads[tid].store = 0;
}

thread_state * get_thread_block(void)
{
  return (thread_state *) get_fs();
}

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
  int myid = *((int *) arg);
  /* setup TLS on startup */
  setup_tls(myid);

  printf("[child] started (%d)\n", myid);

  thread_state *ts = get_thread_block();
  ts->store = 99;

  printf("[child] tid: %d\n", ts->tid);
  printf("[child] store: %d\n", ts->store);

  usleep(500000);
  return 0;
}

int prog(void)
{
  int tid = 0, child_id = 0;

  setup_tls(next_thread_id);
  child_id = ++next_thread_id;

  printf("[main_] started\n");

  tid = new_thread(&child, &child_id);
  printf("[main_] child launched: %d\n", tid);

  thread_state *ts = get_thread_block();
  ts->store = 77;

  printf("[main_] tid: %d\n", ts->tid);
  printf("[main_] store: %d\n", ts->store);

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
