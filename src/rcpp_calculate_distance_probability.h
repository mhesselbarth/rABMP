#ifndef RCPP_CALCULATE_DISTANCE_PROBABILITY_H
#define RCPP_CALCULATE_DISTANCE_PROBABILITY_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_distance_probability(float beta,
                                                  float theta,
                                                  int max_dist);

#endif
