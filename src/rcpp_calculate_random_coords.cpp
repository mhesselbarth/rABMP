#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_calculate_random_coords(double n,
                                           String species,
                                           int n_proposed = 1000000,
                                           double max_dist = 80) {

  // create proposed coordinates values
  NumericVector proposed = runif(n_proposed, 0, max_dist);

  // initialise vector vor kernel probabilities
  NumericVector kernel(n_proposed, 0.0);

  // create target
  NumericVector target = runif(n_proposed, 0, 1);

  // initialse double for integration
  double denominator = 0;

  double alpha = 0;

  if(species == "Beech" | species == "Hornbeam") {
    alpha = 3.412413;
  }

  else if(species == "Ash") {
    alpha = 0.922805;
  }

  else if(species == "Sycamore") {
    alpha = 7.435026;
  }

  else if(species == "others") {
    alpha = 3.795664;
  }

  else {
    proposed = NumericVector::get_na();
    return proposed;
  }

  // integrate area under kernel
  for(int j = 0; j < max_dist; j++) {
    denominator += std::exp(-alpha / std::pow(10, 5) * std::pow(j, 3));
  }

  for(int i = 0; i < proposed.size(); i++) {

    kernel[i] = std::exp(-alpha / std::pow(10, 5) * std::pow(proposed[i], 3));

    kernel[i] = (1 / denominator) * kernel[i];
  }

  proposed = proposed[target <= (kernel / max(kernel))];

  LogicalVector change_id = runif(proposed.size(), 0, 1) <= 0.5;

  NumericVector proposed_negative = proposed[change_id];

  proposed_negative = proposed_negative * -1;

  proposed[change_id] = proposed_negative;

  return sample(proposed, n);
}

// [[Rcpp::export]]
NumericMatrix rcpp_calculate_seedlings(NumericMatrix coords,
                                       NumericVector no,
                                       StringVector species) {

  int nrow = coords.nrow();

  int seedlings_total = sum(no);

  int row_counter = 0;

  NumericMatrix seedlings(seedlings_total, 2);

  for(int i = 0; i < nrow; i++) {

    NumericVector random_x = rcpp_calculate_random_coords(no[i], species[i]);

    NumericVector random_y = rcpp_calculate_random_coords(no[i], species[i]);

    seedlings(row_counter, 0) = coords(i, 0) + random_x[0];

    seedlings(row_counter, 1) = coords(i, 1) + random_y[0];

    row_counter++;
  }

  return seedlings;
}



/*** R
bench::mark(
  rcpp_calculate_random_coords(n = 10, species = "Beech"),
  calculate_random_coords(n = 10, species = "Beech"),
  check = FALSE, relative = TRUE, iterations = 100)
*/
