#' Competition kernel
#'
#' Function to calculate competitionfor c_i experienced by c_j
#' @param pattern [\code{Numeric(1)}]\cr Distance between c_i and c_j
#' @param mark [\code{Numeric(1)}]\cr Size of c_j
#' @param max.dist [\code{Numeric(1)}]\cr Maximum interaction distance. Used to standartize values to 0-1
#' @param type [\code{String(1)}]\cr Kernel type to use (either "Fractional", "Exponential" or "Epanechnikov")
#' @return Vector with CI values for each point
#' @examples
#' CI <- Competition.Index(pattern=pattern_living_trees, DBH="DBH_13", sigma=25)
#'
#' @export
Competition.Kernel <- function(r, dbh, max.dist, type="Epanechnikov"){

  if(type=="Fractional" | type==1){
    alpha <- 3.24074
    beta <- 1.05879
    competition <- (dbh^alpha)/(1+((r/beta)^2))
  }

  else if(type=="Exponential" | type==2){
    alpha <- 1.45772
    beta <- 0.52339
    competition <- (dbh^alpha) * exp(-(r/(dbh^beta)))
  }

  else if(type=="Epanechnikov" | type==3){
    r <- r/max.dist # standarize to max.dist=1
    kernel <- (3/4)*(1-(r^2))
    competition <- kernel * dbh
  }

  else{
    print("Please select valid kernel ('Fractional', 'Exponential' or 'Epanechnikov') - returning CI=0")
    competition <- 0
  }

  return(competition)
}

