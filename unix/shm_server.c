/*
 * shm-server - server program to demonstrate shared memory.
 */
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SHMSZ     27

int main(void)
{
  char c;
  int shmid;
  key_t key;
  char *shm, *s;

  /*
   * We'll name our shared memory segment
   * "5678".
   */
  key = 5678;

  /*
   * Create the segment.
   */
  if ((shmid = shmget(key, SHMSZ, IPC_CREAT | 0666)) < 0) {
    perror("shmget");
    exit(1);
  }

  /*
   * Now we attach the segment to our data space.
   */
  if ((shm = shmat(shmid, NULL, 0)) == (char *) -1) {
    perror("shmat");
    exit(1);
  }

  /*
   * Now put some things into the memory for the
   * other process to read.
   */
  s = shm;
  for (c = 'a'; c <= 'z'; c++) {
    *s++ = c;
  }
  *s = '\0';

  /*
   * Finally, we wait until the other process 
   * changes the first character of our memory
   * to '*', indicating that it has read what 
   * we put there.
   */
  while (*shm != '*') {
    sleep(1);
  }

  /*
   * Shared memory segments out-live the process that
   * created them, we need to * delete explicitly.
   * 
   * Can view live segments with `ipcs`.
   */
  shmctl(shmid, IPC_RMID, NULL);

  exit(0);
}
