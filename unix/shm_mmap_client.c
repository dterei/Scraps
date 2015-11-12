/*
 * shm-mmap-client - client program to demonstrate mmap shared memory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>

#define SHMSZ     27
#define SHMNAME   "/tmp/mymemory"

int main()
{
  int shmid;
  char *shm, *s;

  /*
   * Open the segment.
   */
  if ((shmid = open(SHMNAME, O_RDWR)) < 0) {
    perror("shm_open");
    exit(1);
  }

  /*
   * Map into address space.
   */
  if ((shm = mmap(NULL, SHMSZ,
                  PROT_READ | PROT_WRITE,
                  MAP_FILE | MAP_SHARED, shmid, 0)) == MAP_FAILED) {
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
