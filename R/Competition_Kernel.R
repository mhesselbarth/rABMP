#' Competition kernel
#'
#' Function to calculate competitionfor c_i experienced by c_j
#' @param r [\code{numeric(1)}]\cr Distance between c_i and c_j
#' @param dbh [\code{Numeric(1)}]\cr DBH of c_j
#' @param max_dist [\code{numeric(1)}]\cr Maximum interaction distance.
#' Used to standartize values to 0-1 for Epanechnikov kernel
#' @param type [\code{string(1)}]\cr Kernel type to use (either "Fractional", "Exponential" or "Epanechnikov")
#'
#' @export
Competition.Kernel <- function(r, dbh, max_dist, type="Epanechnikov"){

  if(type=="Fractional"){
    alpha <- 3.24074
    beta <- 1.05879
    competition <- (dbh^alpha)/(1+((r/beta)^2))
  }

  else if(type=="Exponential"){
    alpha <- 1.45772
    beta <- 0.52339
    competition <- (dbh^alpha) * exp(-(r/(dbh^beta)))
  }

  else if(type=="Epanechnikov"){
    r <- r/max_dist # standarize to max.dist=1
    kernel <- (3/4)*(1-(r^2))
    competition <- kernel * dbh
  }

  else{
    print("Please select valid kernel ('Fractional', 'Exponential' or 'Epanechnikov') - returning CI=0")
    competition <- 0
  }

  return(competition)
}

