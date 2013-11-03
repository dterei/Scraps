#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#define NRAND 1000000

#define HIST_SIZE 25
#define HIST_MIN 0
#define HIST_MAX (HIST_SIZE - 1)

int hist[HIST_SIZE];

int main (void)
{
	const gsl_rng_type * T;
	gsl_rng * r;
	double mu = 2.0;

	/* create a generator chosen by the 
	   environment variable GSL_RNG_TYPE */
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);

	printf("Distribution with mean of %f\n", mu);

	/* print N random variates chosen from 
	   the poisson distribution with mean 
	   parameter mu */
	for (int i = 0; i < NRAND; i++) {
		/* unsigned int k = gsl_ran_poisson(r, mu) + 10; */
		unsigned int k = gsl_ran_gaussian(r, mu) + 10;
		if (HIST_MIN <= k && k <= HIST_MAX) {
			hist[k]++;
		} else {
			printf ("outside history range: %u\n", k);
		}
	}

	for (int i = 0; i < HIST_SIZE; i++) {
		printf("[%d] ", i);
		for (int j = 0; j < hist[i] / (NRAND / 100); j++) {
			putchar('*');
		}
		printf("\n");
	}

	gsl_rng_free (r);
	return 0;
}

