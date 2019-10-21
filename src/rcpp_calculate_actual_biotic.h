#ifndef RCPP_CALCULATE_ACTUAL_BIOTIC
#define RCPP_CALCULATE_ACTUAL_BIOTIC

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_actual_biotic(NumericMatrix matrix,
                                           double alpha, double beta, double mod,
                                           int max_dist);

#endif
