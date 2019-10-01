#ifndef RCPP_RANDOM_DISTANCE_H
#define RCPP_RANDOM_DISTANCE_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_random_distance(int number_seeds,
                                   float beta,
                                   int max_dist);

#endif
