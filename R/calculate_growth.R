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
  assymp <- parameters$growth_assymp
  rate <- parameters$growth_rate
  infl <- parameters$growth_infl

  # calculate DBH increase (Pommering et al. 2014 formula 10)
  dbh <- assymp * rate * infl * exp(-rate * dbh) *
    ((1 - exp(-rate * dbh)) ^ (infl - 1))

  return(dbh)
}
