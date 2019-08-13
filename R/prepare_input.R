#' prepare_data
#'
#' @description Prepare data
#'
#' @param data Dataframe with input data.
#' @param x String with name of column containing the x-coordinate.
#' @param y String with name of column containing the y-coordinate.
#' @param type String with name of column containing the type as string.
#' @param species String with name of column containing the species.
#' @param dbh String with name of column containing the DBH as dbl.
#'
#' @details
#' The funcion modifies the structures of the data dataframe. This includes
#' renameing of the coloumns and nesting the data.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' names(example_input_data)
#' prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' }
#'
#' @aliases prepare_data
#' @rdname prepare_data
#'
#' @export
prepare_data <- function(data, x, y, species, type, dbh){

  # convert to tibble
  data <- tibble::as_tibble(data)

  # initialize competition index
  data$ci <- 0

  # initialize time step counter
  data$i <- 0

  # add id
  data$id <- seq(1:nrow(data))

  # order columns
  data <- data[c("id", "i", c(x, y, species, type, dbh), "ci")]

  # name columns
  names(data) <- c("id", "i", "x", "y", "species", "type", "dbh", "ci")

  # # nest data
  # data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
