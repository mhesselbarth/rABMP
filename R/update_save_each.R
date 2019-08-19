#' update_save_each
#'
#' @description Update the timestep
#'
#' @param data Dataframe with input data.
#' @param save_each Integer value specifying time step results are saved.
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
#' update_save_each(data = df_trees)
#' }
#'
#' @aliases update_save_each
#' @rdname update_save_each
#'
#' @export
update_save_each <- function(data, save_each){

  # which ids can be divided by save_each without rest
  id <- data[i %% save_each == 0 | type == "dead", which = TRUE]

  # only keep those rows
  data <- data[id]

  return(data)
}
