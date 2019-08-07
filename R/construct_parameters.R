#' construct_parameters
#'
#' @description Construct parameters
#'
#' @details
#' Construct a list with all default paramerts for run_model. The parameters include:
#' \itemize{
#'   \item First item
#'   \item Second item
#' }
#'
#' @param verbose Logical if true information is printed.
#' @param ... Possibility to change the value of parameters.
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
construct_parameters <- function(verbose = TRUE, ...) {

  # simulate_ci
  ci_alpha <- 1.45772
  ci_beta <- 0.52339
  ci_max_dist <- 30

  # simulate_growth
  growth_mod <- 3.33278

  # calculate_growth
  growth_assymp <- 75.03706
  growth_rate <- 0.02700
  growth_infl <- 3.41053

  # simulate seed_dispersal
  seed_thres <- 30
  seed_empty <- mean(stats::runif(n = 10000, min = 0.812, max = 0.83))
  seed_success <- 0.0236

  # calculate_seeds
  seed_str_beech <- 121.22
  seed_str_ash <- 26.18
  seed_str_sycamore <- 182.42
  seed_str_hornbeam <- 121.22
  seed_str_others <- mean(seed_str_beech, seed_str_ash,
                          seed_str_sycamore, seed_str_hornbeam)

  # simulate mortality (rcpp_calculate_mortality_probs)
  mort_int_beech_early <- 1.8
  mort_dbh_beech_early <- -2.1
  mort_int_beech_late <- -8.9
  mort_dbh_beech_late <- 0.052
  mort_dinc_beech <- -1.4
  mort_int_ash <- 1.3
  mort_dbh_ash <- -1.6
  mort_int_others <- -2.8
  mort_dbh_others <- -0.051

  # combine all parameters to list
  parameters <- mget(ls())

  # get all parameters that should be changed
  parameters_change <- list(...)

  # there are parameters to change
  if (length(parameters_change) > 0) {

    # check if only valid parameters should be changed
    if (!all(names(parameters_change) %in% names(parameters))) {

      stop("Not all provided parameters are present in the model.",
           call. = FALSE)
    }

    # print how many parameters are changed
    if (verbose) {

      # less than three: actually print names
      if (length(parameters_change) <= 5) {

        message("> Changing the following parameter(s): ",
                paste0(names(parameters_change), collapse = ", "))
      }

      # more than 5: only print number of changes
      else {

        message("> Changing ", length(parameters_change), " parameters.")
      }
    }

    # change parameters
    parameters[names(parameters_change)] <- parameters_change
  }

  # no changes
  else {

    if (verbose) {

      message("> Using default values.")
    }
  }

  # remove verbose parameter
  parameters <- parameters[names(parameters) != "verbose"]

  # return result
  return(parameters)
}
