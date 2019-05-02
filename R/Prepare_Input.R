#' prepare_input
#'
#' @description Prepare input

#' @param input input dataframe
#' @param x name of column containing the x-coordinate
#' @param y name of column containing the y-coordinate
#' @param type name of column containing the type as string
#' @param species name of column containing the species
#' @param dbh name of column containing the DBH as dbl
#'
#' @details
#' The function modifies the structures of the input dataframe. This includes renameing
#' of the coloumns and nesting the data.
#'
#' @return tibble
#'
#' @examples
#' names(example_input_data)
#' prepare_input(input = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#'
#' @aliases prepare_input
#' @rdname prepare_input
#'
#' @export
prepare_input <- function(input, x, y, species, type, dbh){

  # convert to tibble
  input <- tibble::as_tibble(input)

  # initialize competition index
  input$ci <- 0

  # initialize time step counter
  input$i <- 0

  # add id
  input$id <- seq(1:nrow(input))

  # order columns
  input <- input[c("id", "i", c(x, y, species, type, dbh), "ci")]

  # name columns
  names(input) <- c("id", "i", "x", "y", "species", "type", "dbh", "ci")

  # nest data
  input <- tidyr::nest(input, -c(id, x, y, species), .key = "data")

  return(input)
}
