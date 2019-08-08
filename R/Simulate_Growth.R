#' simulate_growth
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
#' names(example_input_data)
#' df_tress <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' df_trees <- simulate_ci(data = df_tress)
#' parameters <- construct_parameters()
#' simulate_growth(data = df_trees, parameters = parameters)
#' }
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
simulate_growth <- function(data, parameters){

  # unnest data
  data <- tidyr::unnest(data)

  # data of past time steps
  past <- data[which(data$type == "Dead" | data$i != max(data$i)), ]

  # only get living trees of current timestep
  current <- data[which(data$type != "Dead" & data$i == max(data$i)), ]

  # calculate potential growth
  growth <- rabmp::calculate_growth(dbh = current$dbh,
                                    parameters = parameters)

  # reduce potential growth (Pommerening et al. 2014 formula 12)
  growth <- growth * parameters$growth_mod * (1 - current$ci)

  # update DBH
  current$dbh <- current$dbh + growth

  # update type below dbh <= 10 cm
  current$type[which(current$dbh > 1 & current$dbh <= 10)] <- "Sapling"

  # update type below dbh > 10 cm
  current$type[which(current$dbh > 10)] <- "Adult"

  # combine tibbles
  data <- rbind(current, past)

  # nest tibble
  data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
