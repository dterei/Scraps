/*
 * shm-allocator - allocator using file-backed shared-memory to enable process
 * (and OS) restarts.
 */
#define _GNU_SOURCE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#define PGSIZE    4096
#define SHMSZ     10737418240 // 10GB
/* #define BASEADR   (void *) 0x00000001033d4000 */
#define BASEADR   (void *) 0x00005f3c90200000
/* #define BASEADR   NULL */

struct T {
  int    x;
  char * y;
  struct T * t;
};

int add(int x, int y)
{
  return x + y;
}

int fd_nofile(void)
{
  int shmid;
  /* shm_unlink("myallocator"); */
  if ((shmid = shm_open("myallocator", O_RDWR | O_CREAT, 0666)) < 0) {
    perror("shm_open");
    abort();
  }
  return shmid;
}

int fd_file(void)
{
  int shmid;
  if ((shmid = open("/tmp/myallocator",
                    O_RDWR | O_CREAT | O_NOATIME,
                    0666)) < 0) {
    perror("open");
    abort();
  }
  return shmid;
}

int main(void)
{
  char c;
  int err, shmid;
  char *shm, *s;
  long pgsz;

  if ((pgsz = sysconf(_SC_PAGESIZE)) == -1) {
    perror("sysconf");
    abort();
  } else if (pgsz != PGSIZE) {
    fprintf(stderr, "Unsupported page size! (%ld)\n", pgsz);
    abort();
  }

  /*
   * Open shared memory fd.
   */
  shmid = fd_file();

  /*
   * Size the segment.
   */
  if (ftruncate(shmid, SHMSZ) < 0) {
    perror("ftruncate");
    abort();
  }

  /*
   * Map into address space.
   */
  if ((shm = mmap(BASEADR, SHMSZ,
                  PROT_READ | PROT_WRITE,
                  MAP_FILE | MAP_SHARED | MAP_FIXED,
                  shmid, 0)) == MAP_FAILED) {
    perror("mmap");
    abort();
  }

  if (shm != BASEADR) {
    printf("Address: %p\n", (void *) shm);
    abort();
  }

  /*
   * Check if first time using region?
   */
  if (!shm[0]) {
    printf("First time with memory!\n");
  } else {
    printf("Re-using memory!\n");
    struct T * tptr = (struct T *) (shm+1);
    printf("T1{ %d, %d, %p }\n", tptr->x, *tptr->y, tptr->t);
    printf("T2{ %d, %p, %p }\n", tptr->t->x, tptr->t->y, tptr->t->t);
    memset(shm, 0, 100);
  }

  /*
   * Mark region as active!
   */
  shm[0] = 1;

  /*
   * Store structure.
   */
  struct T * t1 = (struct T *) (shm + 1);
  t1->x = 10;
  t1->y = shm;
  struct T * t2 = (struct T *) t1 + 1;
  t2->x = 20;
  t2->y = shm;
  t2->t = NULL;
  t1->t = t2;

  /*
   * Loop writing.
   */
  s = (char *) (t2 + 1);
  unsigned int i, j;
  for (i = 0; i < 1024*1024*1024; i++) {
    for (j = 0; j < i; j++) {
      s[j] = (char) i;
    }
  }

  /*
   * Close file descriptor for segment.
   */
  if (close(shmid) < 0) {
    perror("close");
    abort();
  }

  return EXIT_SUCCESS;
}
