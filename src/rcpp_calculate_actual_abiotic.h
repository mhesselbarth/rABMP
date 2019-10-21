#ifndef RCPP_CALCULATE_ACTUAL_ABIOTIC
#define RCPP_CALCULATE_ACTUAL_ABIOTIC

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_actual_abiotic(NumericMatrix matrix,
                                            double alpha, double beta, double mod,
                                            double gamma,
                                            int max_dist);

#endif
