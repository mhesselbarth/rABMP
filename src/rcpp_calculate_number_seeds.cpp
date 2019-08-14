#include "rcpp_calculate_number_seeds.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_number_seeds(StringVector species,
                                          NumericVector dbh,
                                          double str_beech,
                                          double str_ash,
                                          double str_sycamore,
                                          double str_hornbeam,
                                          double str_others) {

  // get size of input
  int size_input = species.size();

  // initialise double for number of seeds
  double n;
  double str;

  // initialise vector to store number of seeds
  Rcpp::NumericVector number_seeds(size_input, 0.0);

  // loop through input
  for(int i = 0; i < size_input; i++) {

    // calculate number seeds depending on species
    if (species[i] == "beech") {
      str = str_beech;
    }

    else if (species[i] == "ash") {
      str = str_ash;
    }

    else if (species[i] == "sycamore") {
      str = str_sycamore;
    }

    else if (species[i] == "hornbeam") {
      str = str_hornbeam;
    }

    else if (species[i] == "others") {
      str = str_others;
    }

    n = str * std::pow((dbh[i] / 30), 2);

    // store result in vector
    number_seeds[i] = n;
  }

  return number_seeds;
}

/*** R
parameters <- rabmp::read_parameters("inst/parameters.txt")

species <- rabmp::example_input_data$spec
dbh <- rabmp::example_input_data$bhd

b <- bench::mark(rcpp_calculate_number_seeds(species = species, dbh = dbh,
                                        str_beech = parameters$seed_str_beech,
                                        str_ash = parameters$seed_str_ash,
                                        str_sycamore = parameters$seed_str_sycamore,
                                        str_hornbeam = parameters$seed_str_hornbeam,
                                        str_others = parameters$seed_str_others),
            calculate_number_seeds(species = species, dbh = dbh, parameters = parameters),
            relative = TRUE, iterations = 10000)
*/
