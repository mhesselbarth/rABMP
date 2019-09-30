#' simulate_growth_biotic
#'
#' @description Simulate growth
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
#'
#' @details
#' Simulate growth by calculating the DBH increase and add it to the current DBH.
#' Also, i (iteration counter) will be increase and the type re-classified based
#' on the DBH: trees with a DBH smaller than 10 cm get the type 'Sapling' and trees
#' with a DBH over 10 cm the type 'Adult'.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = "\t")
#'
#' names(example_input_data)
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#'
#' df_trees <- simulate_ci(data = df_trees, parameters = parameters)
#'
#' simulate_growth_biotic(data = df_trees, parameters = parameters)
#' }
#'
#' @aliases simulate_growth_biotic
#' @rdname simulate_growth_biotic
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
simulate_growth_biotic <- function(data, parameters){

  # get id of current living
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # calculate potential growth
  growth <- rabmp::calculate_growth(dbh = data[id, dbh],
                                    parameters = parameters)

  # reduce potential growth (Pommerening et al. 2014 formula 12)
  growth <- growth * parameters$growth_mod * (1 - data[id, ci])

  # update DBH
  data[id, dbh := dbh + growth]

  # update type 1 < dbh <= 10
  data[dbh > 1 & dbh <= 10 & type != "dead", type := "sapling"]

  # update type below dbh > 10
  data[dbh > 10 & type != "dead", type := "adult"]

  return(data)
}
