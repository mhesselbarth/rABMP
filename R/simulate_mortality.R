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
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = "\t")
#'
#' names(example_input_data)
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#'
#' simulate_mortality(data = df_trees)
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
  mortality_prob <- rcpp_calculate_mortality_probs(species = data[id, species],
                                                   dbh = data[id, dbh],
                                                   int_beech_early = parameters$mort_int_beech_early,
                                                   dbh_beech_early = parameters$mort_dbh_beech_early,
                                                   int_beech_late = parameters$mort_int_beech_late,
                                                   dbh_beech_late = parameters$mort_dbh_beech_late,
                                                   dinc_beech = parameters$mort_dinc_beech,
                                                   int_ash = parameters$mort_int_ash,
                                                   dbh_ash = parameters$mort_dbh_ash,
                                                   int_others = parameters$mort_int_others,
                                                   dbh_others = parameters$mort_dbh_others)

  # create random number for all living trees
  random_number <- stats::runif(n = length(mortality_prob), min = 0, max = 1)

  # set all to dead if mortality prob is larger than random number
  id <- id[random_number <= mortality_prob]

  data[id, type := "dead"]

  return(data)
}
