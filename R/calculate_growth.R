#' calculate_growth
#'
#' @description Calculate growth
#'
#' @param dbh Numeric with DBH of target plant.
#'
#' @details
#' Calculates DBH increase (growth) of individual trees based on current DBH.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' dbh <- c(24.3, 65.2, 12.5)
#' calculate_growth(dbh)
#' }
#'
#' @aliases calculate_growth
#' @rdname calculate_growth
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
calculate_growth <- function(dbh){

  # set parameters
  A <- 75.03706
  k <- 0.02700
  p <- 3.41053

  # calculate DBH increase
  dbh <- A * k * p * exp(-k * dbh) * (1 - exp(-k * dbh)) ^ (p - 1)

  return(dbh)
}
