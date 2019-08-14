#include "rcpp_random_distance.h"
#include "rcpp_calculate_distance_probability.h"

// [[Rcpp::export]]
NumericVector rcpp_random_distance(int number_seeds,
                                   String species,
                                   double beta_beech,
                                   double beta_ash,
                                   double beta_sycamore,
                                   double beta_hornbeam,
                                   double beta_others,
                                   int max_dist) {

  // initialise parameters
  double beta = 0.0;
  double theta = 3.0;

  // initialise vector to store distances
  Rcpp::NumericVector distance_vector(number_seeds, 0.0);

  // set parameters depending on species
  if(species == "Beech") {
    beta = beta_beech / std::pow(10, 5);
    // theta = 3;
  }

  else if(species == "Ash") {
    beta = beta_ash / std::pow(10, 5);
    // theta = 3;
  }

  else if(species == "Sycamore") {
    beta = beta_sycamore / std::pow(10, 5);
    // theta = 3;
  }

  else if(species == "Hornbeam") {
    beta = beta_hornbeam / std::pow(10, 5);
    // theta = 3;
  }

  else if(species == "others") {
    beta = beta_others / std::pow(10, 5);
    // theta = 3;
  }

  else{
    stop("Please select valid species");
  }

  // get cumulative probability function
  Rcpp::NumericVector probability = rcpp_calculate_distance_probability(beta, theta, max_dist);

  // loop through all needed seedlings
  for(int i = 0; i < number_seeds; i++) {

    // counter to find probability where probability > random number
    int counter;

    // create random uniform number between 0 - 1
    double random = runif(1, 0, 1)[0];

    // loop through all probabilities and inrease counter
    for(counter = 0; counter < probability.size(); counter++) {

      // break if probability >= random number
      if(probability[counter] >= random)
        break;
    }

    // get id of probability before break
    int counter_prev = counter - 1;

    // calculate distance
    double distance = (counter + (random - probability[counter_prev]) / (probability[counter] - probability[counter_prev]));

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
max_dist <- 120
beta <- 3.412413 / 10 ^ 5
theta <- 3

rcpp_random_distance(number_seeds = 10, species = "Beech", max_dist = 120)

plot(density(rcpp_random_distance(number_seeds = 100000, species = "Beech", max_dist = 120)), lty = 1)
# lines(density(deprecated_rcpp_random_distance(n = 100000, species = "Beech")), lty = 2)

# bench::mark(rcpp_random_distance(number_seeds = 25, species = "Beech", max_dist = 120),
#             deprecated_rcpp_random_distance(n = 25, species = "Beech"),
#             check = FALSE, iterations = 1000, relative = TRUE)
*/
