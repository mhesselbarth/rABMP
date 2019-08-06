#' calculate_growth
#'
#' @description Calculate growth
#'
#' @param dbh Numeric with DBH of target plant.
#' @param parameters List with all parameters.
#'
#' @details
#' Calculates DBH increase (growth) of individual trees based on current DBH.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' dbh <- c(24.3, 65.2, 12.5)
#' parameters <- construct_parameters()
#' calculate_growth(dbh, parameters = parameters)
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
calculate_growth <- function(dbh, parameters){

  # set parameters
  A <- parameters$A
  k <- parameters$k
  p <- parameters$p

  # calculate DBH increase
  dbh <- A * k * p * exp(-k * dbh) * (1 - exp(-k * dbh)) ^ (p - 1)

  return(dbh)
}
