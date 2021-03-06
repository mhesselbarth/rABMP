#ifndef RCPP_CALCULATE_CI_H
#define RCPP_CALCULATE_CI_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_ci(NumericMatrix matrix,
                                float alpha,
                                float beta,
                                int max_dist);

#endif
