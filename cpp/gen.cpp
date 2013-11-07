#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <random>
#include <cmath>

typedef struct {
	std::mt19937 *prng;
	std::normal_distribution<double> *dist;
} dist_normal;

dist_normal* new_normal(double mean, double stddev)
{
	std::random_device rd;

	dist_normal *dn = (dist_normal *) malloc(sizeof(dist_normal));
	dn->prng = new std::mt19937(rd());
	dn->dist = new std::normal_distribution<>(mean, stddev);
	return dn;
}

double normal_rnd(dist_normal *dn)
{
	std::normal_distribution<double> d = *dn->dist;
	double r = std::round(d(dn->prng));
	return r;
}

