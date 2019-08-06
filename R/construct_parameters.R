#' construct_parameters
#'
#' @description Construct parameters
#'
#' @details
#' Construct a list with all default paramerts for run_model.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' construct_parameters()
#' }
#'
#' @aliases construct_parameters
#' @rdname construct_parameters
#'
#' @references
#' Bilek, L., Remes, J., Zahradnik, D., 2009. Natural regeneration of senescent even-
#' aged beech (Fagus sylvatica L.) stands under the conditions of Central Bohemua.
#' Journal of Forest Science 55(4), 145-155
#'
#' Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die -
#' partitioning tree mortality dynamics in a near-natural mixed deciduous forest.
#' J. Ecol. 101, 220–230.
#'
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology.
#' For. Ecol. Manage. 331, 135-143.
#'
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion.
#' Ecology, 75(6), 1794-1806.
#'
#' @export
construct_parameters <- function() {

  # simulate_ci
  alpha <- 1.45772
  beta <- 0.52339
  max_dist <- 30

  # simulate_growth
  v <- 3.33278

  # calculate_growth
  A <- 75.03706
  k <- 0.02700
  p <- 3.41053

  # simulate seed_dispersal
  reproduction_threshold <- 30
  empty_seeds <- mean(stats::runif(n = 10000, min = 0.812, max = 0.83))
  seedling_success <- 0.0236

  # calculate_seeds
  str_beech <- 121.22
  str_ash <- 26.18
  str_sycamore <- 182.42
  str_hornbeam <- 121.22
  str_others <- mean(str_beech, str_ash, str_sycamore, str_hornbeam)

  # simulate mortality (rcpp_calculate_mortality_probs)
  int_beech_early <- 1.8
  dbh_beech_early <- -2.1
  int_beech_late <- -8.9
  dbh_beech_late <- 0.052
  dinc_beech <- -1.4
  int_ash <- 1.3
  dbh_ash <- -1.6
  int_others <- -2.8
  dbh_others <- -0.051

  result <- mget(ls())

  return(result)
}
