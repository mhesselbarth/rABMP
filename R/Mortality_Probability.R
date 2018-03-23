#' Calculcate mortalitay mortality probability (not running)
#'
#' Function to model mortality of trees
#' @param species [\code{character(1)}]\cr Character with species of tree
#' @param dbh [\code{Numeric(1)}]\cr DBH of tree in cm
#' @references \itemize{
#' \item Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230.
#' }
#'
#' @export
Mortality.Probability <- function(species, dbh){

  if(species == "Beech"){
    logit <- -8.9 + (0.052 * dbh)
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Ash"){
    logit <- 1.3 + (-1.6 * log(dbh))
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Hornbeam"){
    logit <- -2.8 + (-0.051 * dbh)
    p <- 1 / (1 + exp(-logit))

  }

  else if(species == "others"){
    logit <- -8.9 + (0.052 * dbh)
    p <- 1 / (1 + exp(-logit))
  }

  else{
    print("Please select valid species - returning 0")
    p <- 0
  }

  return(p)
}

