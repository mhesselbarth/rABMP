#' simulate_mortality
#'
#' @details
#' Function to model mortality of trees
#'
#' @param input Tibble with input data
#' @param threshold Trees with an DBH increase below die
#' @param time_steps Time frame in which DBH increase is considered
#'
#' @references \itemize{
#' \item Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230.
#' }
#'
#' @export
simulate_mortality <- function(input, threshold = 0.25, time_steps = 3){ # Not working currently! Something's wrong in Mortalitaty.Probability()

  # unnest data
  input_unnested <- tidyr::unnest(input)

  # only get living trees of current timestep above threshold
  current_living <- dplyr::filter(input_unnested,
                                  type != "Dead",
                                  i >= max(i) - time_steps)

  # group according to id
  current_living_grouped <- dplyr::group_by(current_living, id)

  # calculate increase
  increase_dbh <- dplyr::summarise(current_living_grouped, increase = abs(sum(diff(dbh))))

  # which trees had an increase below the threshold
  ids_below_thres <- dplyr::filter(increase_dbh, increase < threshold)[[1]]

  # update type to dead of most recent timestep
  updated <- dplyr::mutate(input_unnested, type = dplyr::case_when(id %in% ids_below_thres & i == max(i) ~ "Dead",
                                                                   id %in% ids_below_thres & i != max(i) ~ type,
                                                                   !(id %in% ids_below_thres) ~ type))

  # nest tibble
  result <- tidyr::nest(updated, -c(id, x, y, species), .key = "data")

  return(result)
}

# past <- input %>% # data of previous time steps
#   tidyr::unnest() %>%
#   dplyr::filter(i != max(i))
#
# current <- input %>% # data of current time steps
#   tidyr::unnest() %>%
#   dplyr::filter(i == max(i))
#
# x <- current %>%
#   purrr::pmap_dbl(., function(Species, DBH, ...){rABMP::mortality_probability(species = Species, dbh = DBH)})
#
# for(i in 1:pattern$n){
#   dbh_i <- pattern$marks$DBH[i]
#   species_i <- as.character(pattern$marks$Species[i])
#
#   mortality_prob <- rABMP::mortality_probability(species=species_i, dbh=dbh_i)
#   if(runif(1)<mortality_prob){pattern$marks$Type[i] <- "dead"}
# }
