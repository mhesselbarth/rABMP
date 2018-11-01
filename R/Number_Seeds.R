#' Number of seeds
#'
#' The function calculates the number of seeds for each tree depending on the DBH
#' @param species Species of tree
#' @param dbh DBH of tree
#' @references \itemize{
#' \item Ribbens, E., Silander, J.A., Pacala, S.W., 1994. Seedling recruitment in forests: Calibratiing models to predict patterns of tree seedling dispersion. Ecology 75, 1794–1806.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#'
#' @export
number_seeds <- function(species, dbh){

  if(species == "Beech"){
    no_seeds <- 121.22 * (dbh / 30) ^ 2

    # Based on Milleron et al. (2013) Year 2007
    # beta <- runif(n = 1, min = 0.015, max = 0.017)
    # no_seeds <- exp(beta) * (pi * (dbh/2)^2 / 10000)
  }

  else if(species == "Ash"){no_seeds <- 26.18 * (dbh / 30) ^ 2 +5}

  else if(species == "Sycamore"){no_seeds <- 182.42 * (dbh/30) ^ 2}

  else if(species == "Hornbeam"){no_seeds <- 121.22 * (dbh/30) ^ 2} # same as Beech

  else if(species == "others"){no_seeds <- 112.76 * (dbh / 30) ^ 2} # mean all other species

  else{
    print("Please select valid species - no_seeds=0 returned")
    no_seeds <- 0
  }

   return(no_seeds)
}
