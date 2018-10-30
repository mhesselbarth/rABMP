#' Prepare Input
#'
#' Function to prepare the input dataframe in a tibble with the structure needed by all functions
#' @param input Tibble with input data
#' @param x String with  name of column containing the x-coordinate
#' @param y String with  name of column containing the y-coordinate
#' @param type String with  name of column containing the type as string
#' @param species String with  name of column containing the species
#' @param dbh String with name of column containing the DBH as dbl
#'
#' @export
prepare_input <- function(input, x, y, species, type, dbh){

  input$ci <- 0 # initialize competition index
  input$i <- 0 # initialize time step counter

  input_ordered <- input[c(c(x, y, species, type, dbh), "ci", "i")] # order columns
  names(input_ordered) <- c("x", "y", "species", "type", "dbh", "ci", "i") # name columns

  input_nested <- tidyr::nest(input_ordered, -c(species, x, y), .key = "data") # nest data

  result <- dplyr::mutate(input_nested,
                          data = purrr::map(data, function(input){input <- input[c("i", "type", "dbh", "ci")]})) # order columns of nested data

  return(result)
}
