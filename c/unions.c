#include <stdio.h>

typedef union test
{
	int int_t;
	float float_t;
} test;

int main( void )
{
	test test_t;

	test_t.int_t = 1;

	printf("Test Union: &union test_t = %lx, &int_t = %lx, &float_t = %lx\n",
			&test_t, &test_t.int_t, &test_t.float_t);

	printf("Test Union: int_t = %d, float_t = %f\n",
			test_t.int_t, test_t.float_t);

	test_t.float_t = 2.0f;

	printf("Test Union: int_t = %d, float_t = %f\n",
			test_t.int_t, test_t.float_t);

	printf("Test Union: int_t = %d, float_t = %f\n",
			test_t.int_t, test_t.float_t);

	test_t.int_t = 2;

	printf("Test Union: int_t = %d, float_t = %f\n",
			test_t.int_t, test_t.float_t);

	return 0;
}

