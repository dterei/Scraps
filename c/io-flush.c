#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[])
{
	char buf[1024];

	printf("Enter your name, please: ");
	fgets(buf, 1024, stdin);
	printf("Your name is: %s\n", buf);

	return EXIT_SUCCESS;
}

