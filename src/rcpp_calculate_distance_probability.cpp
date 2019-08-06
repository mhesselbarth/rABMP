#include "rcpp_calculate_distance_probability.h"

// https://github.com/LMurphy186232/Core_Model/blob/9a6cf25a466c9eea346c30e61f9c593c2b72705b/Behaviors/SpatialDisperse.cpp
// Starting line 793

// [[Rcpp::export]]
NumericVector rcpp_calculate_distance_probability(double beta,
                                                  double theta,
                                                  int max_dist) {

  // initialise doubles
  double probability_temp = 0.0;
  double normalizer = 0.0;

  // initialise vector for probabilities
  Rcpp::NumericVector probability(max_dist, 0.0);

  // loop from to max_dist and sum all probabilities (area under curve)
  for(int distance = 0; distance < max_dist; distance++) {

    // normalizer += M_PI * (2 * (distance + 0.5)) * std::exp(-beta * std::pow(distance, theta));
    normalizer += std::exp(-beta * std::pow(distance, theta));
  }

  // normalize area under curve
  normalizer = 1.0 / normalizer;

  // loop again through all distance, calculate probability but use normalizer and store each value
  for(int distance = 0; distance < max_dist; distance++) {

    // probability_temp += normalizer * M_PI * (2 * (distance + 0.5)) * std::exp(-beta * std::pow(distance, theta));
    probability_temp += normalizer * std::exp(-beta * std::pow(distance, theta));

    probability[distance] = probability_temp;
  }

  // probability can't be larger than 1
  probability[probability > 1] = 1;

  // last value in vector must be 1
  probability[probability.size() - 1] = 1;

  return probability;
}

/*** R
max_dist <- 120
beta <- 3.412413 / 10 ^ 5
theta <- 3

rcpp_calculate_probability(beta = beta, theta = theta, max_dist = max_dist)

plot(x = 1:max_dist, y = rcpp_calculate_probability(max_dist = 120, beta = beta, theta = theta), lty = 1, type = "l")
lines(x = 1:max_dist, y = deprecated_calculate_probability(max_dist = 120, beta = beta, theta = theta), lty = 2)

bench::mark(rcpp_calculate_probability(max_dist = max_dist, beta = beta, theta = theta),
            deprecated_calculate_probability(max_dist = max_dist, beta = beta, theta = theta),
            check = FALSE, relative = TRUE, iterations = 100000)
*/
