#ifndef RCPP_CALCULATE_NUMBER_SEEDS_H
#define RCPP_CALCULATE_NUMBER_SEEDS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector rcpp_calculate_number_seeds(StringVector species,
                                          NumericVector dbh,
                                          double str_beech,
                                          double str_ash,
                                          double str_sycamore,
                                          double str_hornbeam,
                                          double str_others);

#endif
