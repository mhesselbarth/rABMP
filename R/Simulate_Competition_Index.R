#' Update competition index
#'
#' The function updates the competition index of the provided pattern using an Epanechnikov kernel. The competition
#' kernel idea is mainly based on Pommering et al. (2011) and Pommering et al. (2014).
#' @param pattern [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' @param dbh [\code{String(1)}]\cr Name of the weights mark
#' @param ci [\code{String(1)}]\cr Name of the competition index
#' @param standardized [\code{Logical(1)}]\cr Standardize maximum CI to 1
#' @param max.dist [\code{Numeric(1)}]\cr Maximum interaction distance between trees
#' @return Updated point pattern
#' @examples
#' CI <- Competition.Index(pattern=pattern_living_trees, dbh="DBH_13", ci="CI")
#'
#' @export
Simulate.Competition.Index <- function(pattern, dbh, ci, standardized=T, max.dist=30){

  if(!(ci %in% names(marks(pattern)))){
    pattern$marks[ci] <- rep(0, pattern$n) # Create CI column if not already present
  }

  pattern_dead <- spatstat::subset.ppp(pattern, Type=="dead")
  pattern_living <- spatstat::subset.ppp(pattern, Type!="dead")

  ci_total <- spatstat::applynbd(pattern_living, R=max.dist, exclude=T, FUN=function(Y, current,...){ # CI considering all points within max.dist
    ci_temp <- vector()
    ci_i <- 0
    if(Y$n>0){ # Check if neighbour present
      for(i in 1:Y$n){ # Loop through all neighbours
        dbh_i <- as.numeric(Y[i]$marks[dbh]) # DBH of neighbour
        dist <- sqrt((current$x - Y[i]$x)^2 + (current$y - Y[i]$y)^2) # Distance to current point
        ci_ij <- Competition.Kernel(r=dist, dbh=dbh_i, max.dist=max.dist) # CI index to neighbour
        ci_i <- ci_i + ci_ij # Total CI of current point
      }
    }
    ci_temp <- c(ci_temp, ci_i) # Vector for all points
    return(ci_temp)
  })

  if(standardized==T){
    ci_total <- ci_total/max(ci_total)
  }

  pattern_living$marks[ci] <- ci_total # Update pattern

  pattern_all <- spatstat::superimpose(pattern_dead, pattern_living, W=pattern$window)

  return(pattern_all)
}

