#' simulate_mortality_abiotic
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
#' @aliases simulate_mortality_abiotic
#' @rdname simulate_mortality_abiotic
#'
#' @references
#' Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die -
#' partitioning tree mortality dynamics in a near-natural mixed deciduous forest.
#' J. Ecol. 101, 220–230.
#'
#' @export
simulate_mortality_abiotic <- function(data, parameters, abiotic_quantiles) {

  # get id of current living
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # get abiotic values
  abiotic_values <- data[id, abiotic]

  # get mortality parameters
  int_early <- ifelse(test = abiotic_values > abiotic_quantiles[2],
                      yes = parameters$mort_int_early_high,
                      no = ifelse(test = abiotic_values < abiotic_quantiles[1],
                                  yes = parameters$mort_int_early_low,
                                  no = parameters$mort_int_early))

  dbh_early <- ifelse(test = abiotic_values > abiotic_quantiles[2],
                      yes = parameters$mort_dbh_early_high,
                      no = ifelse(test = abiotic_values < abiotic_quantiles[1],
                                  yes = parameters$mort_dbh_early_low,
                                  no = parameters$mort_dbh_early))

  int_late <- ifelse(test = abiotic_values > abiotic_quantiles[2],
                      yes = parameters$mort_int_late_high,
                      no = ifelse(test = abiotic_values < abiotic_quantiles[1],
                                  yes = parameters$mort_int_late_low,
                                  no = parameters$mort_int_late))

  dbh_late <- ifelse(test = abiotic_values > abiotic_quantiles[2],
                      yes = parameters$mort_dbh_late_high,
                      no = ifelse(test = abiotic_values < abiotic_quantiles[1],
                                  yes = parameters$mort_dbh_late_low,
                                  no = parameters$mort_dbh_late))

  dinc <- ifelse(test = abiotic_values > abiotic_quantiles[2],
                 yes = parameters$mort_dinc_high,
                 no = ifelse(test = abiotic_values < abiotic_quantiles[1],
                             yes = parameters$mort_dinc_low,
                             no = parameters$mort_dinc))



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
