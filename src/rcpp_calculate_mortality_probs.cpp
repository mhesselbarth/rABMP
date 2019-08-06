#include "rcpp_calculate_mortality_probs.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_mortality_probs(StringVector species,
                                             NumericVector dbh,
                                             double int_beech_early,
                                             double dbh_beech_early,
                                             double int_beech_late,
                                             double dbh_beech_late,
                                             double dinc_beech,
                                             double int_ash,
                                             double dbh_ash,
                                             double int_others,
                                             double dbh_others) {


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

      dbh_inc = std::exp(-3.4 + 2.1 * (1 - std::exp(-(-0.00035) *
        std::pow(dbh[i], 2.5))));

      // test if NA
      dbh_test = NumericVector::is_na(int_beech_early +
        (std::log(dbh[i] + 8) * dbh_beech_early) + (dbh_inc * dinc_beech));

      // set to 0 if NA, if not use value
      if(dbh_test) {
        logit_early = 0;
      } else {
        logit_early = int_beech_early +
          (std::log(dbh[i] + 8) * dbh_beech_early) + (dbh_inc * dinc_beech);
      }

      logit_late = int_beech_late + (dbh[i] * dbh_beech_late);

      p_early = 1 / (1 + std::exp(-logit_early));

      p_late = 1 / (1 + std::exp(-logit_late));

      p = p_early + p_late;
    }

    else if(species[i] == "Ash") {

      logit = int_ash + (std::log(dbh[i]) * dbh_ash);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "Hornbeam") {

      logit = int_others + (dbh[i] * dbh_others);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "Sycamore") {

      logit = int_others+ (dbh[i] * dbh_others);

      p = 1 / (1 + std::exp(-logit));
    }

    else if(species[i] == "others") {

      logit = int_others + (dbh[i] * dbh_others);

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
