#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static
void
usageExit(char *program)
{
	fprintf(stderr, "Usage: %s [file] [length] [char]\n", program);
	exit(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
	if (argc != 4)
	{ usageExit(argv[0]); }

	FILE *fp = fopen(argv[1], "w+");
	int len = atoi(argv[2]);
	char c = argv[3][0];

	if (fp == NULL || len <= 0)
  { usageExit(argv[0]); }

	int i;
	for (i = 0; i < len; i++)
	{
		fwrite(&c, 1, 1, fp);
	}
	
	fflush(fp);
	fclose(fp);
}

