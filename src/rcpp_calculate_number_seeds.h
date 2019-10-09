#ifndef RCPP_CALCULATE_NUMBER_SEEDS_H
#define RCPP_CALCULATE_NUMBER_SEEDS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_number_seeds(NumericVector dbh,
                                          float str);

#endif
