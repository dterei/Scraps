/*
 * shm-posix-client - client program to demonstrate posix shared memory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>

#define SHMSZ     10737418240 // 10GB
#define SHMNAME   "mymemory"
#define BASEADR   (void*) 2147483648 // 2^31, 2GB

int main()
{
  int shmid;
  char *shm, *s;

  /*
   * Open the segment.
   */
  if ((shmid = shm_open(SHMNAME, O_RDWR)) < 0) {
    perror("shm_open");
    exit(1);
  }

  /*
   * Map into address space.
   */
  if ((shm = mmap(BASEADR, SHMSZ,
                  PROT_READ | PROT_WRITE,
                  MAP_FILE | MAP_SHARED | MAP_FIXED,
                  shmid, 0)) == MAP_FAILED) {
    perror("mmap");
    exit(1);
  }

  /*
   * Now read what the server put in the memory.
   */
  for (s = shm; *s != '\0'; s++) {
    putchar(*s);
  }
  putchar('\n');

  /*
   * Finally, change the first character of the 
   * segment to '*', indicating we have read 
   * the segment.
   */
  *shm = '*';

  exit(0);
}
