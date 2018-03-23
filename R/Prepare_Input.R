#' Prepare Input
#'
#' Function to prepare the input dataframe in a tibble with the structure needed by all functions
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @param x [\code{string(1)}]\cr String with  name of column containing the x-coordinate as dbl
#' @param y [\code{string(1)}]\cr String with  name of column containing the y-coordinate as dbl
#' @param type [\code{string(1)}]\cr String with  name of column containing the type as string.
#' \cr The types must be 'Adult', 'Seedlings' and 'Dead'
#' @param species [\code{string(1)}]\cr String with  name of column containing the species
#' @param dbh [\code{string(1)}]\cr String with name of column containing the DBH as dbl
#'
#' @export
Prepare.Input <- function(input, x, y, species, type, dbh){

  input$CI <- 0 # initialize competition index
  input$i <- 0 # initialize time step counter

  input_ordered <- input[c(c(x, y, species, type, dbh), "CI", "i")] # order columns
  names(input_ordered) <- c("x", "y", "Species", "Type", "DBH", "CI", "i") # name columns

  input_nested <- tidyr::nest(input_ordered, -c(Species, x, y), .key = "Data") # nest data

  result <- input_nested %>%
    dplyr::mutate(Data = purrr::map(Data, function(input){input <- input[c("i", "Type", "DBH", "CI")]})) # order columns of nested data

  return(result)
}
