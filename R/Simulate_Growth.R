#' Update DBH
#'
#' The function simulates growth of individual points
#' @param pattern [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' @param year [\code{Numeric(1)}]\cr Year of simulation
#' @param mark [\code{String(1)}]\cr Name of the mark
#' @param ci [\code{String(1)}]\cr Name of the competition index
#' @return Vector with size increase for each point
#' @examples
#' CI <- Competition.Index(pattern=pattern_living_trees, DBH="DBH_13", sigma=25)
#'
#' @export
Simulate.Growth <- function(pattern, year, dbh, ci){

  pattern$marks[paste0("Year_", year-1)] <- pattern$marks[dbh] # Save old DBH value
  pattern_dead <- spatstat::subset.ppp(pattern, Type=="dead")
  pattern_living <- spatstat::subset.ppp(pattern, Type!="dead")

  increase_total <- rep(0, pattern_living$n) # Vector for all points

  for(i in 1:pattern_living$n){ # Loop over all points in pattern
    species <- as.character(pattern_living[i]$marks$Species) # Species of point
    dbh_i <- as.numeric(pattern_living[i]$marks[dbh]) # DBH of point
    ci_factor <- as.numeric(pattern_living[i]$marks[ci]) # Competition factor of point
    increase_i <- Growth.Function.Species(dbh=dbh_i, species=species) # DBH increase of point
    increase_i <- increase_i * (1-(ci_factor)) # Influence of competition on growth
    increase_total[i] <- increase_i # Vector for all points
  }

  pattern_living$marks[dbh] <- pattern_living$marks[dbh] + increase_total # Update DBH

  pattern_all <- spatstat::superimpose(pattern_dead, pattern_living, W=pattern$window)

  return(pattern)
}



