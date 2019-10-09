#include "rcpp_create_seedlings.h"
#include "rcpp_random_distance.h"

// [[Rcpp::export]]
NumericMatrix rcpp_create_seedlings(NumericMatrix coords,
                                    NumericVector number,
                                    float beta,
                                    int max_dist) {

  // get number of trees
  const int nrow = coords.nrow();

  // get number of total seeds
  const int seedlings_total = sum(number);

  // initialise counter because each row will get several new seedlings
  int counter = 0;

  // initialise matrix
  Rcpp::NumericMatrix seedlings(seedlings_total, 2);

  for(int i = 0; i < nrow; i++) {

    // calculate random distances
    Rcpp::NumericVector random_x = rcpp_random_distance(number[i],
                                                        beta,
                                                        max_dist);

    Rcpp::NumericVector random_y = rcpp_random_distance(number[i],
                                                        beta,
                                                        max_dist);

    for(int j = 0; j < number[i]; j++){

      seedlings(counter, 0) = coords(i, 0) + random_x[j];

      seedlings(counter, 1) = coords(i, 1) + random_y[j];

      counter++;
    }
  }

  return seedlings;
}

/*** R
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_create_seedlings(coords = as.matrix(df_trees[, .(x, y)]),
                      number = rep(10, nrow(df_trees)),
                      beta = parameters$seed_beta,
                      max_dist = parameters$seed_max_dist)
*/
