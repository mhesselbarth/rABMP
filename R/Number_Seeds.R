#' Number of seeds
#'
#' The function calculates the number of seeds for each tree depending on the DBH
#' @param species [\code{string(1)}]\cr Species of tree
#' @param dbh [\code{numeric(1)}]\cr DBH of tree
#' @references \itemize{
#' \item Ribbens, E., Silander, J.A., Pacala, S.W., 1994. Seedling recruitment in forests: Calibratiing models to predict patterns of tree seedling dispersion. Ecology 75, 1794–1806.
#' }
#'
#' @export
Number.Seeds <- function(species, dbh){

  if(species == "Beech"){no_seeds <- 121.22 * (dbh / 30) ^ 2}

  else if(species == "Ash"){no_seeds <- 26.18 * (dbh / 30) ^ 2}

  else if(species == "Sycamore"){no_seeds <- 182.42 * (dbh/30) ^ 2}

  else if(species == "Hornbeam"){no_seeds <- 121.22 * (dbh/30) ^ 2} # same as Beech

  else if(species == "others"){no_seeds <- 112.76 * (dbh / 30) ^ 2} # mean all other species

  else{
    print("Please select valid species - no_seeds=0 returned")
    no_seeds <- 0
  }

   return(no_seeds)
}
