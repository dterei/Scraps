/*
 * shm-posix-server - server program to demonstrate posix shared memory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/types.h>

#define SHMSZ     10737418240 // 10GB
#define SHMNAME   "mymemory"
#define BASEADR   (void *) 0x1033d4000

int main(void)
{
  char c;
  int err, shmid;
  char *shm, *s;

  /*
   * Destroy segment.
   */
  shm_unlink(SHMNAME);

  /*
   * Create the segment.
   */
  if ((shmid = shm_open(SHMNAME, O_RDWR | O_CREAT, 0666)) < 0) {
    perror("shm_open");
    exit(1);
  }

  /*
   * Size the segment
   */
  if (ftruncate(shmid, SHMSZ) < 0) {
    perror("ftruncate");
    exit(1);
  }

  /*
   * Map into address space.
   */
  if ((shm = mmap(BASEADR, SHMSZ,
                  PROT_READ | PROT_WRITE,
                  MAP_FILE | MAP_SHARED,
                  shmid, 0)) == MAP_FAILED) {
    perror("mmap");
    exit(1);
  }

  printf("Main: %p\n", (void *) main);
  if (shm != BASEADR) {
    printf("Address: %p\n", (void *) shm);
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
   * Close file descriptor for segment.
   */
  if (close(shmid) < 0) {
    perror("close");
    exit(1);
  }

  exit(0);
}
