#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/stat.h>

/*
 * shmat, shmdt, shmctl, shm_unlink
 * ftruncate, fstat, fchown, fchmod, close
 *
 * shmget -- older, SysV5 standard -- returns a key that must be
 *           communicated to other processes somehow. I believe lifetime
 *           is tied to the process. NO: you can specify the key, so can
 *           have a convention on what value to use and avoid communication.
 *
 * shm_open -- newer, POSIX standard -- open on a key name, so multiple
 *             processes can easily open it. Must be unlinked to kill
 *             shared memory (i.e., lifetime is outside the process) using
 *             shm_unlink.
 *
 *             Size of shared memory region is initially zero and ftruncate
 *             must be used to change it.
 *
 * mmap -- can use the mmap system call to create shared memory. Lifetime is
 *         tied to the processes using it. Must specify a file path
 *         (i.e., a real path). Specify the MAP_SHARED flag. OR can use
 *         shm_open instead of open to back with a temporary file.
 *
 * /dev/shm/ -- shared memory objects are created in (tmpfs) virtual file
 *              system at /dev/shm. Can use ACL to control access.
 *
 * mincore, madvise, msync, minherit
 * mmap, munmap, mprotect, mlock, munlock
 * getpagesize
 *
 * Typically access to shared memory must be synchronized.
 * Different ways of 'opening' shared memory can give you either process,
 * kernel or filesystem persistence.
 */

#define SHD_NAME "testing-terei12aaadxx"
#define SHD_SIZE 1000
#define SHD_KEY 123

int* X = NULL;
int shd = 0;

inline void print_pid(void)
{
	printf("==============================\n");
	printf("Starting share test...\n");
	printf("My pid is %d and parents is %d\n", getpid(), getppid());
}

// Use UNIX older standard functions of shmget, shmat.
void mk_shared_unix(void)
{
	void* loc;

	// can generate key with ftok instead...
	shd = shmget(SHD_KEY, SHD_SIZE, SHM_R | SHM_W | IPC_CREAT);
	if (shd < 0) {
		perror("Error openning shared memory!");
		exit(1);
	}

	loc = shmat(shd, NULL, 0);
	if (loc == MAP_FAILED) {
		perror("Error openning shared memory!");
		exit(1);
	}
	printf("Shared memory initialized...%p\n", loc);

	X = (int*) loc;
}

// Use POSIX standard functions of shm_open and mmap.
void mk_shared_posix(void)
{
	void* loc;
	struct stat stat;
	int r;

	shd = shm_open(SHD_NAME, O_RDWR | O_CREAT, 0x777);
	if (shd < 0) {
		perror("Error openning shared memory!");
		exit(1);
	}

	// on first time we need to ftruncate to set size... other time it is
	// an error to set the size -- as it has already been set.
	if (fstat(shd, &stat)) {
		perror("Stat Failed...");
		exit(1);
	} else if (stat.st_size == 0) {
		r = ftruncate(shd, SHD_SIZE);
		if (r != 0) {
			perror("Truncae Failed...");
			exit(1);
		}
	}

	loc = mmap(0, SHD_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, shd, 0);
	if (loc == MAP_FAILED) {
		perror("Error openning shared memory!");
		exit(1);
	}
	printf("Shared memory initialized...%p\n", loc);

	X = (int*) loc;
}

void shut_program_unix(void)
{
	shmdt(X);
	// NOTE: can use `ipcrm` on the command line to also kill
	// and `ipcs` to view the status...
	// XXX: Doesn't work! invlaid arguments returned...
	if (shmctl (shd, IPC_RMID, (void *) X) == -1) {
		perror("error in closing segment");
		exit(1);
	}
	exit(0);
}

void shut_program_posix(void)
{
	// shm_open & mmap don't seem to work with the `ipcrm`
	// and `ipcs` commands.
	munmap(X, SHD_SIZE);
	close(shd);
	shm_unlink(SHD_NAME);
	exit(0);
}

int main(int argc, char *argv[])
{
	int n;

	if (argc > 1) {
		n = atoi(argv[1]);
		if (n >= 5) {
			shut_program_posix();
		}
	}

	print_pid();
	mk_shared_posix();

	*X += 1;
	printf("X is %d\n", *X);

	getc(stdin);

	#define N_SIZE 100
	char n_str[N_SIZE];
	snprintf(n_str, N_SIZE, "%d", n + 1);
	close(shd);
	execl("./shared", "shared", n_str, (char *) 0);

	printf("WE SHOULD NEVER EXECUTE THIS!\n");
	return 0;
}

