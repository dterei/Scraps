#include <stdio.h>

int doubleAdd( int x, int y ) {
	return 7 * (x + y);
}

int main(void) {
	int r = doubleAdd(2, 3);
	printf("r = %d\n", r);
	return 0;
}

