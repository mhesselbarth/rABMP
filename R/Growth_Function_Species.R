#' Growth function
#'
#' Internal help function that contains all growing functions for each species
#' @param dbh [\code{numeric(1)}]\cr DBH of target plant
#' @param species [\code{string(1)}]\cr Name of the species
#' @return Numeric with size increase for each point
#'
#' @export
growth_function_species <- function(dbh, species){

  if(species == "Beech"){increase <- dbh * 0.005367 + 0.017436}

  else if(species == "Ash"){increase <- dbh * 0.00534 + 0.12778}

  else if(species == "Hornbeam"){increase <- dbh * 0.004008 - 0.052462}

  else if(species == "Sycamore"){increase <- dbh * 0.004856 - 0.083668}

  else if(species == "others"){increase <- dbh * 0.004100 + 0.002149}

  else{print("Warning: No growth function for species availaibe - increase=0")}

  return(increase)
}

# Wrong functions, calculated for 1999-2007 and not just  one year
