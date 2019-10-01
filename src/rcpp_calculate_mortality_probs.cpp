#include "rcpp_calculate_mortality_probs.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_mortality_probs(NumericVector dbh,
                                             float int_early,
                                             float int_late,
                                             float dbh_early,
                                             float dbh_late,
                                             float dinc) {


  // get size of input
  const int size_input = dbh.size();

  // initialise all needed objects
  float logit_early;

  // initialise vector to store probabilities
  Rcpp::NumericVector probs(size_input, 0.0);

  // loop through input
  // calculate probability depending on species
  for(int i = 0; i < size_input; i++) {

    // calculate dbh increase
    const float dbh_inc = std::exp(-3.4 + 2.1 * (1 - std::exp(-0.00035 * std::pow(dbh[i], 2.5))));

    // test if NA
    const bool dbh_test = NumericVector::is_na(int_early + (std::log(dbh[i] + 8) * dbh_early) + (dbh_inc * dinc));

    // set to 0 if NA, if not use value
    if(dbh_test) {

      logit_early = 0;
    }

    else {

      logit_early = int_early + (std::log(dbh[i] + 8) * dbh_early) + (dbh_inc * dinc);
    }

    const float logit_late = int_late + (dbh[i] * dbh_late);

    const float p_early = 1 / (1 + std::exp(-logit_early));

    const float p_late = 1 / (1 + std::exp(-logit_late));

    const float p = p_early + p_late;

    // store result in vector
    probs[i] = p;
  }

  return probs;
}

/*** R
df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
                         type = "Class", dbh = "bhd")

parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")

rcpp_calculate_mortality_probs(dbh = df_trees$dbh,
                               int_early = parameters$mort_int_early,
                               int_late = parameters$mort_int_late,
                               dbh_early = parameters$mort_dbh_early,
                               dbh_late = parameters$mort_dbh_late,
                               dinc = parameters$mort_dinc)
*/
