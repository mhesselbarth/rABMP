#include "rcpp_calculate_ci.h"

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci(NumericMatrix matrix,
                                double alpha,
                                double beta,
                                double max_dist) {

  // get number of rows
  int nrow = matrix.nrow();

  // initialise vector for distances
  double distance = 0.0;

  // initialise double for temp ci value
  double ci_temp = 0.0;

  // initialise vector for ci value
  Rcpp::NumericVector ci(nrow, 0.0);

  // loop through all rows
  for(int i = 0; i < nrow; i++){
    for(int j = 0; j < nrow; j++){

      // get distance between current point i and point j
      distance = std::sqrt(std::pow(matrix(i, 0) - matrix(j, 0), 2) + std::pow(matrix(i, 1) - matrix(j, 1), 2));

      // row itself or distance greater than max_dist
      if(distance == 0 || distance > max_dist) {
        ci_temp = 0.0;
      } else {
        // calculate ci of current j
        ci_temp = std::pow(matrix(j, 2), alpha) * std::exp(-(distance / std::pow(matrix(j, 2), beta)));
      }

      // add to overall ci if i
      ci[i] += ci_temp;
    }
  }

  return ci;
}

// [[Rcpp::export]]
NumericVector rcpp_calculate_distance(double current_x, double current_y,
                                      NumericVector other_x, NumericVector other_y){

  Rcpp::NumericVector current_x_vector(other_x.size(), current_x);
  Rcpp::NumericVector current_y_vector(other_y.size(), current_y);

  NumericVector distance = sqrt(pow(other_x - current_x_vector, 2) + pow(other_y - current_y_vector, 2));

  return(distance);
}

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci_sugar(NumericMatrix matrix,
                                      double alpha,
                                      double beta,
                                      double max_dist) {

  // get number of rows
  int nrow = matrix.nrow();

  // initialise double for current coordinates
  double current_x;
  double current_y;

  // initialise vector for all other coordinates
  NumericVector other_x(nrow);
  NumericVector other_y(nrow);

  // initialise vector for DBH
  NumericVector dbh = matrix(_, 2);

  // initialise vector for temp DBH
  NumericVector dbh_temp(nrow, 0.0);

  // initialise vector for distances
  NumericVector distance(nrow, 0.0);

  // initialise double for temp ci value
  NumericVector ci_temp = 0.0;

  // initialise vector for ci value
  NumericVector ci(nrow, 0.0);

  // loop through all points
  for(int i = 0; i < nrow; i++){

    // get current coordinates
    current_x = matrix(i, 0);
    current_y = matrix(i, 1);

    // get all other coordinates
    other_x = matrix(_, 0);
    other_y = matrix(_, 1);

    // calculate distance matrix
    distance = rcpp_calculate_distance(current_x, current_y,
                                       other_x, other_y);

    // get all dbh
    dbh_temp = dbh;

    // which distances are below max_dist and not to itself
    LogicalVector id = (distance < max_dist) & (distance != 0);

    // subset data
    distance = distance[id];
    dbh_temp = dbh[id];

    // calculate ci of current tree
    ci_temp = pow(dbh_temp, alpha) * exp(-(distance / pow(dbh_temp, beta)));

    // sum all ci and save to vector
    ci[i] = sum(ci_temp);
  }

  return ci;
}

/*** R
data <- as.matrix(rabmp::example_input_data[1:10, c(1,2,4)])

rcpp_calculate_distance(current_x = data[1, 1], current_y = data[1, 2],
                        other_x = data[-1, 1], other_y = data[-1, 2])

rcpp_calculate_ci(matrix = data, alpha = 1.45772, beta = 0.52339, max_dist = 30)

bench::mark(rcpp_calculate_ci(data, alpha = 1.45772, beta = 0.52339, max_dist = 30),
            deprecated_calculate_ci(data, alpha = 1.45772, beta = 0.52339, max_dist = 30),
            iterations = 1000, relative = TRUE)

*/
