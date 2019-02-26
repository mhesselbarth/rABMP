#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcpp_calculate_seedlings_fast(NumericMatrix coords,
                                            NumericVector number,
                                            StringVector species) {

  // get number of trees
  int nrow = coords.nrow();

  // get number of total seeds
  int seedlings_total = sum(number);

  // initialise counter because each row will get several new seeldings
  int counter = 0;

  // initialise matrix
  NumericMatrix seedlings(seedlings_total, 2);

  for(int i = 0; i < nrow; i++) {

    // calculate probability
    // probability = rcpp_calculate_probability(max_dist, beta, theta)

    // NumericVector random_x = rcpp_calculate_random_coords(number[i], species[i]);
    //
    // NumericVector random_y = rcpp_calculate_random_coords(number[i], species[i]);

    // for(int j = 0; j < number[i]; j++){
    //
    //   seedlings(counter, 0) = coords(i, 0) + random_x[j];
    //
    //   seedlings(counter, 1) = coords(i, 1) + random_y[j];
    //
    //   counter++;
    // }
  }

  return seedlings;
}

/*** R

*/
