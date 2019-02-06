#' Update DBH
#'
#' The function simulates growth of individual points
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @return Vector with size increase for each point
#'
#' @export
simulate_growth <- function(input){

  # unnest data
  input_unnested <- tidyr::unnest(input)

  # only get living trees of current timestep
  current_living <- dplyr::filter(input_unnested, type != "Dead" & i == max(i))

  # calculate growth
#  growth <- purrr::map_dbl(current_living$dbh,
 #                         function(x) rABMP::growth_function_species(dbh = x))
  # growth <- apply(current_living$dbh, 1,  FUN = growth_function_species(dbh = x))

  growth <- growth_function_species(dbh = current_living$dbh)

  # current_living$pot.growth <- growth
  # v <- 3.33278
  # current_living$growth_inc <- current_living$pot.growth * v * (1-current_living$ci)
  # plot(current_living$dbh, current_living$pot.growth, xlab="dbh", ylab="dbh increment", col="darkred", ylim=c(0,1.5))
  # points(current_living$dbh, current_living$growth_inc)
  # legend("topright", legend=c("potential growth", "actual growth"), col=c("darkred", "black"),pch=1)
  # current_living$higher <- current_living$growth_inc >  current_living$pot.growth
  # summary(current_living$higher)
  # plot(y[higher=="FALSE"] ~ x[higher=="FALSE"], data=current_living)
  # points(y[higher=="TRUE"] ~ x[higher=="TRUE"], data=current_living, col="red")

  # update tibble
  v <- 3.33278 # for exponential type
  current_living <- dplyr::mutate(current_living,
                                  i = i + 1, # update timestep
                                  dbh = dbh + growth * v *(1-ci), # add increase to current DBH
                                  type = dplyr::case_when(dbh <= 10 ~ "Sapling", # update type
                                                         dbh > 10 ~ "Adult"))

  # combine tibbles
  full_updated <- dplyr::bind_rows(current_living, input_unnested)


  # nest tibble
  result <- tidyr::nest(full_updated, -c(id, x, y, species), .key = "data")

  return(result)
}
