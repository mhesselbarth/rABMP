#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_calculate_mortality_probs(StringVector species,
                                             NumericVector dbh) {


  // get size of input
  int size_input = species.size();

  // initialise all needed objects
  double logit = 0.0;
  double logit_early = 0.0;
  double logit_late = 0.0;

  double p = 0.0;
  double p_early = 0.0;
  double p_late = 0.0;

  double dbh_inc = 0.0;
  bool dbh_test;

  // initialise vector to store probabilities
  Rcpp::NumericVector probs(size_input, 0.0);

  // loop through input
  for(int i = 0; i < size_input; i++){

    // calculate probability depending on species
    if(species[i] == "Beech") {

      dbh_inc = std::exp(-3.4 + 2.1 * (1 - std::exp(-(-0.00035) * std::pow(dbh[i], 2.5))));

      // test if NA
      dbh_test = NumericVector::is_na(1.8 + (-2.1) * std::log(dbh[i] + 8) + (dbh_inc * -1.4));

      // set to 0 if NA, if not use value
      if(dbh_test) {
        logit_early = 0;
      } else {
        logit_early = 1.8 + (-2.1) * std::log(dbh[i] + 8) + (dbh_inc * -1.4);
      }

      logit_late = -8.9 + (dbh[i] * 0.052);

      p_early = 1 / (1 + std::exp(-logit_early));

      p_late = 1 / (1 + std::exp(-logit_late));

      p = p_early + p_late;
    }

    else if(species[i] == "Ash") {

      logit = 1.3 + (std::log(dbh[i]) * -1.6);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "Hornbeam") {

      logit = -2.8 + (dbh[i] * -0.051);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "Sycamore") {

      logit = -2.8 + (dbh[i] * -0.051);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "others") {

      logit = -2.8 + (dbh[i] * -0.051);

      p = 1 / (1 + std::exp(-logit));
    }

    // store result in vector
    probs[i] = p;
  }

  return probs;
}

/*** R
species <- rabmp::example_input_data$spec
dbh <- rabmp::example_input_data$bhd

rcpp_calculate_mortality_probs(species = species, dbh = dbh)

vec_deprecated_calculate_mortality_probability <- Vectorize(deprecated_calculate_mortality_probability)

bench::mark(rcpp_calculate_mortality_probs(species = species, dbh = dbh),
            vec_deprecated_calculate_mortality_probability(species = species, dbh = dbh),
            check = FALSE, relative = TRUE, iterations = 1000)

*/
