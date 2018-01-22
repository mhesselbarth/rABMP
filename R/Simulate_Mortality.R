#' Mortality function
#'
#' Function to model mortality of trees
#' @param pattern [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' @param dbh [\code{String(1)}]\cr Name of the DBH mark
#' @references \itemize{
#' \item Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230.
#' }
#' @examples
#' Enter Example
#'
#' @export
Simulate.Mortality <- function(pattern, dbh="DBH"){

  for(i in 1:pattern$n){
    dbh_i <- pattern$marks$DBH[i]
    species_i <- as.character(pattern$marks$Species[i])

    mortality_prob <- Mortality.Probability(species=species_i, dbh=dbh_i)
    if(runif(1)<mortality_prob){pattern$marks$Type[i] <- "dead"}
  }

  return(pattern)
}



?log
