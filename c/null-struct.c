#include <stdio.h>

struct teststruct {
	int i;
	int *j;
};

int main(int argc, char *argv[]) {

	struct teststruct ts = {1};
	struct teststruct *tsp = &ts;

	printf("Not Null: %d\n", tsp->i);

	tsp = NULL;
	tsp->j = NULL;
	printf("OK!\n");

	printf("Null: %d\n", tsp->i);
	
	return 0;
}

