#ifndef RCPP_RANDOM_DISTANCE_H
#define RCPP_RANDOM_DISTANCE_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_random_distance(int number_seeds,
                                   String species,
                                   double beta_beech,
                                   double beta_ash,
                                   double beta_sycamore,
                                   double beta_hornbeam,
                                   double beta_others,
                                   int max_dist);

#endif
