#include "rcpp_calculate_distance_probability.h"

// https://github.com/LMurphy186232/Core_Model/blob/9a6cf25a466c9eea346c30e61f9c593c2b72705b/Behaviors/SpatialDisperse.cpp
// Starting line 793

// [[Rcpp::export]]
NumericVector rcpp_calculate_distance_probability(float eta,
                                                  float theta,
                                                  int max_dist) {

  // initialise doubles
  float probability_temp = 0.0;
  float normalizer = 0.0;

  // initialise vector for probabilities
  Rcpp::NumericVector probability(max_dist, 0.0);

  // loop from to max_dist and sum all probabilities (area under curve)
  for(int distance = 0; distance < max_dist; distance++) {

    // normalizer += M_PI * (2 * (distance + 0.5)) * std::exp(-eta * std::pow(distance, theta));
    normalizer += std::exp(-eta * std::pow(distance, theta));
  }

  // normalize area under curve
  normalizer = 1.0 / normalizer;

  // loop again through all distance, calculate probability but use normalizer and store each value
  for(int distance = 0; distance < max_dist; distance++) {

    // probability_temp += normalizer * M_PI * (2 * (distance + 0.5)) * std::exp(-eta * std::pow(distance, theta));
    probability_temp += normalizer * std::exp(-eta * std::pow(distance, theta));

    probability[distance] = probability_temp;
  }

  // probability can't be larger than 1
  probability[probability > 1] = 1;

  // last value in vector must be 1
  probability[probability.size() - 1] = 1;

  return probability;
}

/*** R
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_calculate_distance_probability(eta = parameters$seed_eta,
                                    theta = 3,
                                    max_dist = parameters$seed_max_dist)

*/
