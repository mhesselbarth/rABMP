#' #' Mortality function (not running)
#' #'
#' #' Function to model mortality of trees
#' #' @param pattern [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' #' @param dbh [\code{String(1)}]\cr Name of the DBH mark
#' #' @references \itemize{
#' #' \item Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230.
#' #' }
#' #'
#' #' @export
#' simulate_mortality <- function(input){ # Not working currently! Something's wrong in Mortalitaty.Probability()
#'
#'   past <- input %>% # data of previous time steps
#'     tidyr::unnest() %>%
#'     dplyr::filter(i != max(i))
#'
#'   current <- input %>% # data of current time steps
#'     tidyr::unnest() %>%
#'     dplyr::filter(i == max(i))
#'
#'   x <- current %>%
#'     purrr::pmap_dbl(., function(Species, DBH, ...){rABMP::mortality_probability(species = Species, dbh = DBH)})
#'
#'   for(i in 1:pattern$n){
#'     dbh_i <- pattern$marks$DBH[i]
#'     species_i <- as.character(pattern$marks$Species[i])
#'
#'     mortality_prob <- rABMP::mortality_probability(species=species_i, dbh=dbh_i)
#'     if(runif(1)<mortality_prob){pattern$marks$Type[i] <- "dead"}
#'   }
#'
#'   return(pattern)
#' }
