#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
	int a = 1;

	printf("A = %d\n", a);
	if (a) {
		printf("A = true;\n");
	} else {
		printf("A = false;\n");
	}

	if (!a) {
		printf("!A = true;\n");
	} else {
		printf("!A = false;\n");
	}

	if (!!a) {
		printf("!!A = true;\n");
	} else {
		printf("!!A = false;\n");
	}

	return 0;
}

