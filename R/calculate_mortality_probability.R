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
calculate_mortality_probability <- function(species, dbh){

  if(species == "Beech"){

    # calculate dbh increase
    dbh_inc <- exp(-3.4 + 2.1 * (1 - exp(-(-0.00035) * dbh ^ 2.5 ) ) )

    # calculate logit for "early phase"
    logit_early <- ifelse(test = is.na(1.8 + (-2.1)*log(dbh + 8) + (dbh_inc * -1.4)),
                          yes = 0, no = 1.8 + (-2.1)*log(dbh + 8) + (dbh_inc * -1.4))

    # calculate logit for late phase
    logit_late <- -8.9 + (dbh * 0.052)

    # sum both logits
    p_early <- 1 / (1 + exp(-logit_early))
    p_late <- 1 / (1 + exp(-logit_late))

    # calculate prob
    p <- p_early + p_late
  }

  else if(species == "Ash"){

    # calculate logit
    logit <- 1.3 + (log(dbh) * -1.6)

    # calculate prob
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Hornbeam"){

    # calculate logit
    logit <- -2.8 + (dbh * -0.051)

    # calculate prob
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "Sycamore"){

    # calculate logit
    logit <- -8.9 + (dbh * 0.052)

    # calculate prob
    p <- 1 / (1 + exp(-logit))
  }

  else if(species == "others"){

    # calculate logit
    logit <- -8.9 + (dbh * 0.052)

    # calculate prob
    p <- 1 / (1 + exp(-logit))
  }

  else{
    print("Please select valid species - returning 0")
    p <- 0
  }

  return(p)
}
