#include "rcpp_calculate_ci.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci(NumericMatrix matrix,
                                 float alpha,
                                 float beta,
                                 int max_dist) {

  // get number of rows
  int nrow = matrix.nrow();

  // initialise vector for ci value
  Rcpp::NumericVector ci(nrow, 0.0);

  // loop through all rows
  for(int i = 0; i < nrow; i++){

    for(int j = 0; j < nrow; j++){

      // get distance between current point i and all points j
      const float dist_x = matrix(i, 0) - matrix(j, 0);
      const float dist_y = matrix(i, 1) - matrix(j, 1);

      const float distance = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      // distance to itself or above max_dist
      if(i == j || distance > max_dist)
        continue; // nothing to do...

      // calculate ci of current j
      const float ci_temp = std::pow(matrix(j, 2), alpha) * std::exp(-(distance / std::pow(matrix(j, 2), beta)));

      // add to overall ci of i
      ci[i] += ci_temp;
    }

    // normalize between 0 - 1 and scale with own dbh
    ci[i] = ci[i] / (std::pow(matrix(i, 2), alpha) + ci[i]);
  }

  return ci;
}

/*** R
n <- 10000

df_trees <- data.frame(x = runif(n = n, min = 0, max = 100),
                       y = runif(n = n, min = 0, max = 100),
                       dbh = runif(n = n, min = 5, max = 65))

rcpp_calculate_ci(as.matrix(df_trees), alpha = 1.5, beta = 0.5, max_dist = 30)
*/
