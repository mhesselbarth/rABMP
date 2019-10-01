#' simulate_mortality
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
#' @examples
#' \dontrun{
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#'  type = "Class", dbh = "bhd")
#'
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")
#'
#' simulate_mortality(data = df_trees, parameters = parameters)
#' }
#'
#' @references
#' Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die -
#' partitioning tree mortality dynamics in a near-natural mixed deciduous forest.
#' J. Ecol. 101, 220–230.
#'
#' @export
simulate_mortality <- function(data, parameters) {

  # get id of current living
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # calculate mortality prob (Holzwarth et al. 2013 formula S12, formula 1/2)
  mortality_prob <- rcpp_calculate_mortality_probs(dbh = data[id, dbh],
                                                   int_early = parameters$mort_int_early,
                                                   dbh_early = parameters$mort_dbh_early,
                                                   int_late = parameters$mort_int_late,
                                                   dbh_late = parameters$mort_dbh_late,
                                                   dinc = parameters$mort_dinc)

  # create random number for all living trees
  random_number <- stats::runif(n = length(mortality_prob), min = 0, max = 1)

  # set all to dead if mortality prob is larger than random number
  id <- id[random_number <= mortality_prob]

  data[id, type := "dead"]

  return(data)
}
