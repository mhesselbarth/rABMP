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
  for(int i = 0; i < nrow - 1; i++){

    for(int j = i + 1; j < nrow; j++){

      // get distance between current point i and all points j
      const float dist_x = matrix(i, 0) - matrix(j, 0);
      const float dist_y = matrix(i, 1) - matrix(j, 1);

      const float distance = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      // distance to itself or above max_dist
      if(distance > max_dist)
        continue; // nothing to do...

      // calculate ci of current i and j
      const float ci_temp_i = std::pow(matrix(j, 2), alpha) * std::exp(-(distance / std::pow(matrix(j, 2), beta)));
      const float ci_temp_j = std::pow(matrix(i, 2), alpha) * std::exp(-(distance / std::pow(matrix(i, 2), beta)));

      // increase ci at point i and j
      ci[i] += ci_temp_i;
      ci[j] += ci_temp_j;
    }

    // normalize between 0 - 1 and scale with own dbh
    ci[i] = ci[i] / (std::pow(matrix(i, 2), alpha) + ci[i]);
  }

  // normalize last ci
  ci[nrow - 1] = ci[nrow - 1] / (std::pow(matrix(nrow - 1, 2), alpha) + ci[nrow - 1]);

  return ci;
}

/*** R
set.seed(42)
n <- 10000

df_trees <- data.frame(x = runif(n = n, min = 0, max = 100),
                       y = runif(n = n, min = 0, max = 100),
                       dbh = runif(n = n, min = 5, max = 65))

rcpp_calculate_ci(as.matrix(df_trees), alpha = 1.5, beta = 0.5, max_dist = 30)
*/
