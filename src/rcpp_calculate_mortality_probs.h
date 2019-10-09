#ifndef RCPP_CALCULATE_MORTALITY_PROBS_H
#define RCPP_CALCULATE_MORTALITY_PROBS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_mortality_probs(NumericVector dbh,
                                             float int_early,
                                             float dbh_early,
                                             float int_late,
                                             float dbh_late,
                                             float dinc);

#endif
