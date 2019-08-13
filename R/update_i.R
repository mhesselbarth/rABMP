#' update_i
#'
#' @description Update the timestep
#'
#' @param data Dataframe with input data.
#' @param increase Numeric how much i is increased.
#'
#' @details
#' Small function to increase the time step i after each simulated time step
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#'
#' update_i(data = df_trees)
#' }
#'
#' @aliases update_i
#' @rdname update_i
#'
#' @export
update_i <- function(data, increase = 1){

  # # unnest data
  # data <- tidyr::unnest(data)

  # data of past time steps
  # past <- data[which(data$type == "Dead" & data$i != max(data$i)), ]

  # data of current time step
  current <- data[which(data$type != "Dead" & data$i == max(data$i)), ]

  # update i
  current$i <- current$i + increase

  # combine tibbles
  data <- rbind(current, data)

  # # nest tibble
  # data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  # return df
  return(data)
}

