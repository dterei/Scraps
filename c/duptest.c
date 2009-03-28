#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char *argv[])
{
	FILE *fp1 = fopen("duptestfile", "w");
	if (fp1 == NULL) {
		fprintf(stderr, "Can't Open file!");
		exit(EXIT_FAILURE);
	}

	fprintf(fp1, "FP1\n");
	fflush(fp1);

	int fd2 = dup(fileno(fp1));
	FILE *fp2 = fdopen(fd2, "2");
	if (fp2 == NULL) {
		fprintf(stderr, "Dup failed!\n");
		exit(EXIT_FAILURE);
	}

	fprintf(fp2, "FP2-1\n");
	fflush(fp1);
	close(fp1);

	fprintf(fp2, "FP2-2\n");
	fflush(fp2);
	close(fp2);
	
	//int fd1 = open("duptestfile", O_WRONLY);
	//write(fd1, "FD1\n", 4);

	//int fd2 = dup(fd1);

	return 0;
}

