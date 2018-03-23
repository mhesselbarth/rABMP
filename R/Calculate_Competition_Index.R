#' Calculate competition index
#'
#' Internal help function to calculate the CI for a focal tree to use with purrr::map()
#' @param distance [\code{numeric(n)}]\cr Vector with distances to all neighbouring trees n
#' @param i [\code{numeric(1)}]\cr Counter of the focal tree
#' @param dbh [\code{numeric(n)}] Vector with DBHs of all neighbouring trees n. Will be indexed by i
#' @param max_dist [\code{numeric(1)}]\cr Maximum interaction distance between trees
#' @param type [\code{string(1)}]\cr Kernel type to use (either "Fractional", "Exponential" or "Epanechnikov")
#'
#' @export
Calculate.Competition.Index <- function(distance, i, dbh, max_dist, type = "Epanechnikov"){
  sum(dplyr::if_else(distance < max_dist, # check for all distances within max_dist
                  true = dplyr::if_else(distance == 0, # distance == 0 is the diagonal of the distance matrix
                                      true = 0, # no competition on itself
                                      false = rABMP::Competition.Kernel(distance = distance, # calculate CI index
                                                                      dbh = dbh[i],
                                                                      max_dist = max_dist)),
                  false = 0)) # distance above max_dist
}
