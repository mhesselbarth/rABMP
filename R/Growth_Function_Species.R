#' Growth function
#'
#' Internal help function that contains all growing functions for each species
#' @param dbh DBH of target plant
#' @return Numeric with size increase for each point
#'
#' Chapman-Richards growth function, parameter from Pommerening: Differences between competition
#' kernels and traditional size-ration based competition indices used in forest ecology
#'
#' @export
growth_function_species <- function(dbh){



  A <- 75.03706
  k <- 0.02700
  p <- 3.41053

  increase <- A*k*p*exp(-k*dbh)*(1-exp(-k*dbh))^(p-1)


  return(increase)
}


