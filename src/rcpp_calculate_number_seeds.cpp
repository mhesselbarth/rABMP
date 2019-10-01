#include "rcpp_calculate_number_seeds.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_number_seeds(NumericVector dbh,
                                          float str) {

  // get size of input
  const int size_input = dbh.size();

  // initialise double for number of seeds
  // double n;

  // initialise vector to store number of seeds
  Rcpp::NumericVector number_seeds(size_input, 0.0);

  // loop through input
  for(int i = 0; i < size_input; i++) {

    const float n = str * std::pow((dbh[i] / 30), 2);

    // store result in vector
    number_seeds[i] = n;
  }

  return number_seeds;
}

/*** R
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_calculate_number_seeds(dbh = df_trees$dbh,
                            str = parameters$seed_str)

*/
