#ifndef RCPP_CALCULATE_MORTALITY_PROBS_H
#define RCPP_CALCULATE_MORTALITY_PROBS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_mortality_probs(StringVector species,
                                             NumericVector dbh,
                                             double int_beech_early,
                                             double dbh_beech_early,
                                             double int_beech_late,
                                             double dbh_beech_late,
                                             double dinc_beech);

#endif