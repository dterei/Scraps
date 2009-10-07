#include <stdio.h>

char* test = "\"\'@hello\n\r\tWord!\'~";

int main(void)
{
	printf("Test = %s\n", test);
	return 0;
}

