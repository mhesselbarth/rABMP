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

  # only get living trees of current timestep
  current_living <- data[which(data$type != "Dead" & data$i == max(data$i)), ]

  # calculate growth
  growth <- rabmp::calculate_growth(dbh = current_living$dbh,
                                    parameters = parameters)

  # for exponential type
  v <- parameters$v

  # update DBH reduced by ci
  current_living$dbh <- current_living$dbh + growth * v * (1 - current_living$ci)

  # update type below dbh <= 10 cm
  current_living$type[which(current_living$dbh <= 10)] <- "Sapling"

  # update type below dbh > 10 cm
  current_living$type[which(current_living$dbh > 10)] <- "Adult"

  # update timestep
  current_living$i <- current_living$i + 1

  # combine tibbles
  data <- rbind(current_living, data)

  # nest tibble
  data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
