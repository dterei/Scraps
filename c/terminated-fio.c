#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <file>\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	FILE *fp = fopen(argv[1], "r");
	if (fp == NULL) {
		fprintf(stderr, "Error opening file: %s\n", argv[1]);
		exit(EXIT_FAILURE);
	}

	char buf[4];
	char *rval;

	rval = fgets(buf, 4, fp);

	printf("buf = %s, rval = %p\n", buf, rval);
	printf("buf[3] = %d\n", buf[3]);

	int c = fread(buf, sizeof(char), 4, fp);
	//buf[3] = '\0';

	printf("buf = %s, c = %d\n", buf, c);
	printf("buf[3] = %d\n", buf[3]);

	return EXIT_SUCCESS;
}

