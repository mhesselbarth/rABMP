#include "rcpp_calculate_ci.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci(NumericMatrix matrix,
                                double alpha,
                                double beta,
                                int max_dist) {

  // get number of rows
  int nrow = matrix.nrow();

  // initialise vector for ci value
  Rcpp::NumericVector ci(nrow, 0.0);

  // initialise double for temp ci value
  double ci_temp = 0.0;

  // initialise doubles for distance
  double dist_x = 0.0;
  double dist_y = 0.0;
  double distance = 0.0;

  // loop through all rows
  for(int i = 0; i < nrow; i++){

    for(int j = 0; j < nrow; j++){

      // get distance between current point i and point j
      dist_x = matrix(i, 0) - matrix(j, 0);
      dist_y = matrix(i, 1) - matrix(j, 1);

      distance = std::sqrt(dist_x * dist_x + dist_y * dist_y);

      // row itself or distance greater than max_dist
      if(distance == 0 || distance > max_dist) {
        ci_temp = 0.0;
      } else {
        // calculate ci of current j
        ci_temp = std::pow(matrix(j, 2), alpha) * std::exp(-(distance / std::pow(matrix(j, 2), beta)));
      }

      // add to overall ci if i
      ci[i] += ci_temp;
    }
  }

  return ci;
}

/*** R
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         species = "spec", type = "Class", dbh = "bhd")

data <- as.matrix(df_trees[1:5, c(3, 4, 7)])

rcpp_calculate_ci(data, alpha = 1.45772, beta = 0.52339, max_dist = 30)
*/
