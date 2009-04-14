#include <stdio.h>
#include <assert.h>

int
main(int argc, char *argv[])
{
	assert("OK!");
	assert(! "DON'T PANIC!");
	return 0;
}

