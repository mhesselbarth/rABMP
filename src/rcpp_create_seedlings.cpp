#include "rcpp_create_seedlings.h"
#include "rcpp_random_distance.h"

// [[Rcpp::export]]
NumericMatrix rcpp_create_seedlings(NumericMatrix coords,
                                    NumericVector number,
                                    StringVector species) {

  // get number of trees
  int nrow = coords.nrow();

  // get number of total seeds
  int seedlings_total = sum(number);

  // initialise counter because each row will get several new seeldings
  int counter = 0;

  // initialise matrix
  NumericMatrix seedlings(seedlings_total, 2);

  for(int i = 0; i < nrow; i++) {

    // calculate random distances
    Rcpp::NumericVector random_x = rcpp_random_distance(number[i], species[i], 120);

    Rcpp::NumericVector random_y = rcpp_random_distance(number[i], species[i], 120);

    for(int j = 0; j < number[i]; j++){

      seedlings(counter, 0) = coords(i, 0) + random_x[j];

      seedlings(counter, 1) = coords(i, 1) + random_y[j];

      counter++;
    }
  }

  return seedlings;
}

/*** R
coords <- matrix(data= c(1, 4, 2, 6, 8,
                         5, 8, 3, 7, 1), ncol = 2)
number <- c(5, 1, 7, 4, 2)
species <- c("Beech", "Beech", "Ash", "Hornbeam", "Sycamore")

rcpp_create_seedlings(coords = coords, number = number, species = species)

bench::mark(rcpp_create_seedlings(coords = coords, number = number, species = species),
            deprecated_rcpp_create_seedlings(coords = coords, number = number, species = species),
            check = FALSE, relative = TRUE, iterations = 100)
*/
