#' simulate_mortality_biotic
#'
#' @description Simulate mortality
#'
#' @details
#' Function to model mortality of trees based on logistic regression. The mortality
#' probability depends on the species and the DBH.
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
#'
#' @return data.table
#'
#' @aliases simulate_mortality_biotic
#' @rdname simulate_mortality_biotic
#'
#' @references
#' Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die -
#' partitioning tree mortality dynamics in a near-natural mixed deciduous forest.
#' J. Ecol. 101, 220–230.
#'
#' @export
simulate_mortality_biotic <- function(data, parameters) {

  # get id of current living
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # rep parameters so can differ for abiotic vesion
  int_early <- rep(x = parameters$mort_int_early, time = length(id))
  dbh_early <- rep(x = parameters$mort_dbh_early, time = length(id))

  int_late <- rep(x = parameters$mort_int_late, time = length(id))
  dbh_late <- rep(x = parameters$mort_dbh_late, time = length(id))

  dinc <- rep(x = parameters$mort_dinc, time = length(id))

  # calculate mortality prob (Holzwarth et al. 2013 formula S12, formula 1/2)
  mortality_prob <- rcpp_calculate_mortality_probs(dbh = data[id, dbh],
                                                   int_early = int_early,
                                                   dbh_early = dbh_early,
                                                   int_late = int_late,
                                                   dbh_late = dbh_late,
                                                   dinc = dinc)

  # create random number for all living trees
  random_number <- stats::runif(n = length(mortality_prob), min = 0, max = 1)

  # set all to dead if mortality prob is larger than random number
  id <- id[random_number < mortality_prob]

  data[id, type := "dead"]

  return(data)
}
