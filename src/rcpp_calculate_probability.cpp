#include <Rcpp.h>
using namespace Rcpp;

// https://github.com/LMurphy186232/Core_Model/blob/9a6cf25a466c9eea346c30e61f9c593c2b72705b/Behaviors/SpatialDisperse.cpp
// Starting line 793

// [[Rcpp::export]]
NumericVector rcpp_calculate_probability(int max_dist, double beta, double theta) {

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
r_calculate_probability <- function(max_dist, beta, theta) {

  probability_temp <- 0

  probability <- rep(NA, max_dist)

  # normalizer <- integrate(function(r) {pi * (2 * (r + 0.5)) * exp(-beta * r ^ theta)}, lower = 0, upper = Inf)$value
  normalizer <- integrate(function(r) {exp(-beta * r ^ theta)}, lower = 0, upper = Inf)$value

  normalizer <- ifelse(test = normalizer < 0.0001,
                       yes = 0, no = 1.0 / normalizer)

  for(distance in 1:max_dist) {

    # probability_temp <- probability_temp + normalizer * pi * (2 * (distance + 0.5)) * exp(-beta * distance ^ theta)
    probability_temp <- probability_temp + normalizer * exp(-beta * distance ^ theta)

    probability[[distance]] <- probability_temp
  }

  # prob can't be larger than 1
  probability[which(probability > 1)] <- 1

  # last value in vector should be 1
  probability[length(probability)] <- 1

  return(probability)
}

max_dist <- 120
beta <- 3.412413 / 10 ^ 5
theta <- 3

rcpp_calculate_probability(max_dist = max_dist, beta = beta, theta = theta)

plot(x = 1:max_dist, y = r_calculate_probability(max_dist = 120, beta = beta, theta = theta), type = "l", lty = 1)
lines(x = 1:max_dist, y = rcpp_calculate_probability(max_dist = 120, beta = beta, theta = theta), lty = 2)

bench::mark(r_calculate_probability(max_dist = max_dist, beta = beta, theta = theta),
            rcpp_calculate_probability(max_dist = max_dist, beta = beta, theta = theta),
            check = FALSE, relative = TRUE, iterations = 100000)
*/
