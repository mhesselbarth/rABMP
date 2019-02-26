#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_random_distance(int number_seeds, NumericVector probability) {

  // initialsie vector to store distances
  NumericVector distance_vector(number_seeds, 0.0);

  // loop through all needed seedlings
  for(int i = 0; i < number_seeds; i++) {

    // counter to find probability where probability > random number
    int counter;

    // create random uniform number between 0 - 1
    double random = runif(1, 0, 1)[0];

    // loop through all probabilities and inrease counter
    for(counter = 0; counter < probability.size(); counter++) {

      // break if probability > random number
      if(probability[counter] > random)
        break;
    }

    // get id of probability before break
    int counter_prev = counter - 1;

    // calculate distance
    double distance = (counter + (random - probability[counter_prev]) / (probability[counter] - probability[counter_prev]));

    // store distance
    distance_vector[i] = distance;
  }

  // change id to modify half of distance ti be negative
  LogicalVector change_id = runif(distance_vector.size(), 0, 1) <= 0.5;

  // get aprox half of all distances
  NumericVector distance_vector_negative = distance_vector[change_id];

  // change to negative value
  distance_vector_negative = distance_vector_negative * -1;

  // store back in original vector
  distance_vector[change_id] = distance_vector_negative;

  return distance_vector;
}

/*** R
max_dist <- 120
beta <- 3.412413 / 10 ^ 5
theta <- 3

prob <- rcpp_calculate_probability(max_dist = max_dist, beta = beta, theta = theta)

rcpp_random_distance(number_seeds = 10, probability = prob)

plot(density(rcpp_calculate_random_coords(n = 100000, species = "Beech")), lty = 1)
lines(density(rcpp_random_distance(number_seeds = 100000, probability = prob)), lty = 2)

bench::mark(rcpp_random_distance(number_seeds = 100000, probability = prob),
            rcpp_calculate_random_coords(n = 100000, species = "Beech"),
            check = FALSE, iterations = 100, relative = TRUE)
*/
