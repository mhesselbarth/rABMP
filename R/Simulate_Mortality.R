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
simulate_mortality <- function(input) {

  input_unnested <- tidyr::unnest(input)

  current_living <- dplyr::filter(input_unnested, type != "Dead", i == max(i))

  mortality_prob <- current_living %>% purrr::pmap_dbl(., function(species, dbh, ...) {mortality_probability(species = species, dbh = dbh)})



   # current_living$mortality <-mortality_prob
  # current_living$mortality <- mortality_prob
  #   plot(mortality[species=="Beech"] ~ dbh[species=="Beech"], data = current_living, col ="black",
  #       ylim=c(0, 0.1), xlab="dbh", ylab="mortality probability")
  #  points(mortality[species=="Ash"] ~ dbh[species=="Ash"], data = current_living, col ="darkred")
  #  points(mortality[species=="Hornbeam"] ~ dbh[species=="Hornbeam"], data = current_living, col ="darkgreen")
  #  legend("topright", legend=c("Beech", "Ash", "Hornbeam"), col=c("black", "darkred", "darkgreen"), pch=1)
  #

  n <- length(mortality_prob)
  random <- runif(n)
  current_living$dead <- random <  mortality_prob
  current_living <- dplyr::mutate(current_living,
                                  type = dplyr::case_when(dead == TRUE ~ "Dead",
                                                          dead != TRUE ~ type))

  current_living <- subset(current_living, select = c(id, type) )


  combined <- dplyr::left_join(input_unnested, current_living, by = "id")

  for(x in 1:length(combined$id)) {
    if(is.na(combined$type.y[x])==T) (combined$type.y[x] = combined$type.x[x]) #check for NAs
  }

  for(a in 1:length(combined$id)) {
    if(combined$type.x[a] != combined$type.y[a] && combined$i[a]==max(combined$i)) (combined$type.x[a] = combined$type.y[a]) #update type for dead trees
  }

  updated <- subset(combined, select = -c(type.y)) # delete second type column
  names(updated)[6] <- paste("type") # replace type.x by type
  result <- tidyr::nest(updated, -c(id, x, y, species), .key = "data")


  return(result)


}




