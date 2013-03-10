#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int X = 0;

inline void print_pid(void)
{
	printf("==============================\n");
	printf("Starting exec test...\n");
	printf("My pid is %d and parents is %d\n", getpid(), getppid());
}

int main(int argc, char *argv[])
{
	int n;
	if (argc > 1) {
		printf("Arg 0: %s\n", argv[0]);
		printf("Arg 1: %s\n", argv[1]);
		n = atoi(argv[1]);
		if (n >= 5) {
			exit(0);
		}
	}

	print_pid();

	X += 1;
	printf("X is %d\n", X);

	#define N_SIZE 100
	char n_str[N_SIZE];
	snprintf(n_str, N_SIZE, "%d", n + 1);
	execl("./execv", "execv", n_str, (char *) 0);
	printf("WE SHOULD NEVER EXECUTE THIS!\n");
	return 0;
}

