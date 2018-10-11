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
mortality_probability <- function(species, dbh){


  if(species == "Beech"){
    dbh_inc <- exp(-3.4 + 2.1 * (1 - exp(-(-0.00035) * dbh ^ 2.5)))

    logit_early <- dplyr::if_else(condition = is.na(1.8 + log(dbh * -2.1 + 8) + (dbh_inc * -1.4)),
                                  true = 0, false = 1.8 + log(dbh * -2.1 + 8) + (dbh_inc * -1.4))
    # p_early <- 1 / ((1 + exp(-logit_early)) ^ 8)

    logit_late <- -8.9 + (dbh * 0.052)

    logit_total <- logit_early + logit_late

    p <- 1 / (1 + exp(-logit_total))

  }

  else if(species == "Ash"){
    logit <- 1.3 + (log(dbh) * -1.6)
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Hornbeam"){
    logit <- -2.8 + (dbh * -0.051)
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Sycamore"){
    logit <- -8.9 + (dbh * 0.052)
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "others"){
    logit <- -8.9 + (dbh * 0.052)
    p <- 1 / (1 + exp(-logit))
  }

  else{
    print("Please select valid species - returning 0")
    p <- 0
  }

  return(p)
}
