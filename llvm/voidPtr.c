#include <stdio.h>

char *s = "Hello World";

void mputs(void*);

int main(void)
{
	void *a = (void*) 1232131;
	void *b = (void*) s;

	mputs(b);
	
	return 0;
}

