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
#' @return data.table
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

  # data of current time step
  current <- data[type != "dead" & i == max(i)]

  # update/increase timestep
  current[, i := i + increase]

  # combine tibbles with all data
  data <- rbind(data, current)

  return(data)
}

