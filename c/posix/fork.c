#include <unistd.h>
#include <stdio.h>

// separate processes -- so heap not shared
int x = 0;
long start_pos = 0;

// however, file handles (kernel resident) are shared
FILE *f = NULL;

void parent(void)
{
	long pos;

	printf("[parent] I'm the parent!\n");
	x--;
	printf("[parent] Global x = %d\n", x);
	fseek(f, 100, SEEK_CUR);
	pos = ftell(f);
	printf("[parent] File is at %ld\n", pos);
}

void child(void)
{
	long pos;

	printf("[child] I'm the child!\n");
	x++;
	printf("[child] Global x = %d\n", x);

	pos = ftell(f);
	printf("[child] File is at %ld\n", pos);
	for (int i = 0; i < 100; i++) {
		pos = ftell(f);
		if (pos != start_pos) {
			printf("[child] File moved from %ld to %ld by it %d\n",
				start_pos, pos, i);
			break;
		}
	}
}

int main(void)
{
	printf("Starting fork...\n");
	printf("Global x = %d\n", x);

	f = fopen("/tmp/x", "r");
	start_pos = ftell(f);
	printf("File is at %ld\n", start_pos);

	// parent = !0 (pid of child)
	// child = 0
	if (fork()) {
		parent();
	} else {
		child();
	}

	return 0;
}

