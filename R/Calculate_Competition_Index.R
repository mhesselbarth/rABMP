#' calculate_competition_index
#'
#' @details
#' Function to calculate competition
#'
#' @param distance Vector of distances between focal tree and all others
#' @param dbh Vector with dbh in same order as distance
#' @param max_dist Maximum interaction distance. Used to standartize values to 0-1 for Epanechnikov kernel
#' @param type Kernel type to use (either "fractional", "exponential" or "epanechnikov")
#'
#' @export
calculate_competition_index <- function(distance, dbh, max_dist, type = "exponential"){

  if(type == "fractional"){

    alpha <- 3.24074
    beta <- 1.05879
    competition <- (dbh ^ alpha) / (1 + ((distance / beta) ^ 2))

    competition[which(distance > max_dist | distance == 0)] <- 0



  }

  else if(type == "exponential"){

    alpha <- 1.45772
    beta <- 0.52339
    competition <- (dbh ^ alpha) * exp( - (distance / (dbh ^ beta)))

    competition[which(distance > max_dist | distance == 0)] <- 0


  }

  else if(type == "epanechnikov"){

    distance_standardized <- distance / max_dist # standarize to max_dist=1
    kernel <- (3 / 4) * (1 - (distance_standardized ^ 2))
    competition <- kernel * dbh

    competition[which(distance > max_dist | distance == 0)] <- 0
  }

  else{
    print("Please select valid kernel ('Fractional', 'Exponential' or 'Epanechnikov') - returning CI=0")
    competition <- 0
  }

  return(competition)
}

