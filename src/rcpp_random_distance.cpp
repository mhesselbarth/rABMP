#include "rcpp_random_distance.h"
#include "rcpp_calculate_distance_probability.h"

// [[Rcpp::export]]
NumericVector rcpp_random_distance(int number_seeds,
                                   float beta,
                                   int max_dist) {

  // initialise parameters
  const float theta = 3.0;

  // initialise vector to store distances
  Rcpp::NumericVector distance_vector(number_seeds, 0.0);

  const float beta_scl = beta / std::pow(10, 5);

  // get cumulative probability function
  Rcpp::NumericVector probability = rcpp_calculate_distance_probability(beta_scl, theta, max_dist);

  // loop through all needed seedlings
  for(int i = 0; i < number_seeds; i++) {

    // counter to find probability where probability > random number
    int counter;

    // create random uniform number between 0 - 1
    const float random = runif(1, 0, 1)[0];

    // loop through all probabilities and inrease counter
    for(counter = 0; counter < probability.size(); counter++) {

      // break if probability >= random number
      if(probability[counter] >= random)
        break;
    }

    // get id of probability before break
    const int counter_prev = counter - 1;

    // calculate distance
    const float distance = (counter + (random - probability[counter_prev]) / (probability[counter] - probability[counter_prev]));

    // store distance
    distance_vector[i] = distance;
  }

  // change id to modify half of distance to be negative
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
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_random_distance(number_seeds = 10,
                     beta = parameters$seed_beta,
                     max_dist = parameters$seed_max_dist)
*/
