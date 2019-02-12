#include <Rcpp.h>
using namespace Rcpp;

// Calculate ci index

// [[Rcpp::export]]
NumericVector rcpp_calculate_ci(NumericMatrix matrix, double max_dist,
                                double alpha, double beta) {

  // get number of rows
  int nrow = matrix.nrow();

  // initialise vector for distances
  double distance;

  // initialise double for temp ci value
  double ci_temp = 0.0;

  // initialise vector for ci value
  NumericVector ci(nrow, 0.0);

  // set parameters
  // double alpha = 1.45772;
  // double beta = 0.52339;

  // loop through all rows
  for(int i = 0; i < nrow; i++){
    for(int j = 0; j < nrow; j++){

      // get distance between current point i and point j
      distance = std::sqrt(std::pow(matrix(i, 0) - matrix(j, 0), 2) + std::pow(matrix(i, 1) - matrix(j, 1), 2));

      // row itself or distance greater than max_dist
      if(distance == 0 || distance > max_dist) {
        ci_temp = 0;
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

// // [[Rcpp::export]]
// NumericVector rcpp_calculate_distance(double current_x, double current_y,
//                                       NumericVector other_x, NumericVector other_y){
//
//   NumericVector current_x_vector(other_x.size(), current_x);
//   NumericVector current_y_vector(other_y.size(), current_y);
//
//   NumericVector distance = sqrt(pow(other_x - current_x_vector, 2) + pow(other_y - current_y_vector, 2));
//
//   return(distance);
// }

// // [[Rcpp::export]]
// NumericVector rcpp_calculate_ci_sugar(NumericMatrix matrix, double max_dist) {
//
//   // get number of rows
//   int nrow = matrix.nrow();
//
//   // initialise double for current coordinates
//   double current_x;
//   double current_y;
//
//   // initialise vector for all other coordinates
//   NumericVector other_x(nrow);
//   NumericVector other_y(nrow);
//
//   // initialise vector for DBH
//   NumericVector dbh = matrix(_, 2);
//
//   // initialise vector for temp DBH
//   NumericVector dbh_temp(nrow, 0.0);
//
//   // initialise vector for distances
//   NumericVector distance(nrow, 0.0);
//
//   // initialise double for temp ci value
//   NumericVector ci_temp = 0.0;
//
//   // initialise vector for ci value
//   NumericVector ci(nrow, 0.0);
//
//   // set parameters
//   double alpha = 1.45772;
//   double beta = 0.52339;
//
//   // loop through all points
//   for(int i = 0; i < nrow; i++){
//
//     // get current coordinates
//     current_x = matrix(i, 0);
//     current_y = matrix(i, 1);
//
//     // get all other coordinates
//     other_x = matrix(_, 0);
//     other_y = matrix(_, 1);
//
//     // calculate distance matrix
//     distance = rcpp_calculate_distance(current_x, current_y,
//                                        other_x, other_y);
//
//     // get all dbh
//     dbh_temp = dbh;
//
//     // which distances are below max_dist and not to itself
//     LogicalVector id = (distance < max_dist) & (distance != 0);
//
//     // subset data
//     distance = distance[id];
//     dbh_temp = dbh[id];
//
//     // calculate ci of current tree
//     ci_temp = pow(dbh_temp, alpha) * exp(-(distance / pow(dbh_temp, beta)));
//
//     // sum all ci and save to vector
//     ci[i] = sum(ci_temp);
//   }
//
//   return ci;
// }

/*** R
r_calculate_ci <- function(data, max_dist) {

  ci_r <- rep(NA, nrow(data))

  for(i in 1:nrow(data)) {

    # calculate distance between current point and all other points
    distance <- calculate_distance(point_a = data[i, 1:2, drop = FALSE],
                                   point_b = data[, 1:2])

    dbh <- data[which(distance < max_dist & distance != 0), 3]

    distance <- distance[which(distance < max_dist & distance != 0)]

    # calculate competition of current tree
    ci_r[i] <- sum(calculate_ci(distance = distance,
                                dbh = dbh,
                                max_dist = max_dist,
                                type = "exponential"))
  }
  return(ci_r)
}

data <- as.matrix(rabmp::example_input_data[, c(1,2,4)])

bench::mark(rcpp_calculate_ci(data, max_dist = 30,
                              alpha = 1.45772, beta = 0.52339),
            # rcpp_calculate_ci_sugar(data, max_dist = 30),
            r_calculate_ci(data, max_dist = 30),
            iterations = 100, relative = TRUE)

*/


