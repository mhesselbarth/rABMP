#ifndef RCPP_CREATE_SEEDLINGS_H
#define RCPP_CREATE_SEEDLINGS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix rcpp_create_seedlings(NumericMatrix coords,
                                    NumericVector number,
                                    StringVector species);

#endif
