#ifndef RCPP_CALCULATE_MORTALITY_PROBS_H
#define RCPP_CALCULATE_MORTALITY_PROBS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_mortality_probs(NumericVector dbh,
                                             NumericVector int_early,
                                             NumericVector dbh_early,
                                             NumericVector int_late,
                                             NumericVector dbh_late,
                                             NumericVector dinc);

#endif
