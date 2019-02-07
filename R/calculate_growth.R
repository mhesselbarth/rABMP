#' calculate_growth
#'
#' @description Calculate growth
#'
#' @param dbh DBH of target plant
#'
#' @details
#' Calculates DBH increase (growth) of individual trees based on current DBH
#'
#' @return vector
#'
#' @examples
#' dbh <- c(24.3, 65.2, 12.5)
#' calculate_growth(dbh)
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
  increase <- A * k * p * exp(-k * dbh) * (1 - exp(-k * dbh)) ^ (p - 1)

  return(increase)
}



#' calculate_ci
#'
#' @description Calculate competition index
#'
#' @param distance distance between focal tree and all others
#' @param dbh DBH of trees
#' @param max_dist maximum interaction distance
#' @param type kernel type to use (either "fractional", "exponential" or "epanechnikov")
#'
#' @details
#' Calculate competition index based on distances to neighbouring trees and dbh of
#' neighbours. Neigbours with a distance larger than \code{max_dist} are not considered.
#' It is used to standartize values to 0-1 for Epanechnikov kernel
#'
#' @return vector
#'
#' @examples
#' add examples
#'
#' @aliases calculate_ci
#' @rdname calculate_ci
#'
#' @references
#' Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence
#' of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666–678.
#'
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135–143.
#'
