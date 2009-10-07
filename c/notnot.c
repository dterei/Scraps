#include <stdio.h>

void printBool(int a)
{

	printf("A = %d\n", a);

	if (a) {
		printf("A = true;\n");
	} else {
		printf("A = false;\n");
	}

}

int main(int argc, char *argv[])
{
	int d = 2;
	int a = 1;
	int b = 0;
	int c = -1;

	printBool(a);
	printBool(b);
	printBool(c);
	printBool(d);

	return 0;
}

