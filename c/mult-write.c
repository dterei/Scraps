#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s [w|r]\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	FILE *fp = fopen("testfile", argv[1]);
	if (fp == NULL) {
		fprintf(stderr, "Error opening file: testfile\n");
		exit(EXIT_FAILURE);
	}

	if (strcmp("w", argv[1]) == 0) {
		int i;
		for (i = 0; i < 100; i++) {
			fprintf(fp, "%3d: AAAAAAAAAAAAAAAAAAAAAAAA\n", i);
		}
	} else if (strcmp("r", argv[1]) == 0) {
		char buf[1024];
		fgets(buf, 1024, fp);
		printf("%s", buf);
	} else {
		fprintf(stderr, "Usage: %s [w|r]\n", argv[0]);
		fclose(fp);
		exit(EXIT_FAILURE);
	}

	
	while (1) {
	}

	fclose(fp);
	return EXIT_SUCCESS;
}

