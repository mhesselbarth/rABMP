#include "rcpp_calculate_ci.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci(NumericMatrix matrix,
                                float alpha,
                                float beta,
                                int max_dist) {

  // get number of rows
  const int nrow = matrix.nrow();

  // initialise vector for ci value
  Rcpp::NumericVector ci(nrow, 0.0);

  // loop through all rows
  for(int i = 0; i < nrow - 1; i++){

    for(int j = i + 1; j < nrow; j++){

      // get distance between current point i and all points j
      const float dist_x = matrix(i, 0) - matrix(j, 0);
      const float dist_y = matrix(i, 1) - matrix(j, 1);

      const float distance = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      // distance above max_dist
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
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_calculate_ci(as.matrix(df_trees[, .(x, y, dbh)]),
                  alpha = parameters$ci_alpha, beta = parameters$ci_beta,
                  max_dist = parameters$ci_max_dist)
*/
