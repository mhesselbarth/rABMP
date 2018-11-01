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
  growth <- purrr::map2_dbl(current_living$species, current_living$dbh,
                            function(x, y) rABMP::growth_function_species(species = x,
                                                                          dbh = y))

  # update tibble
  current_living <- dplyr::mutate(current_living,
                                  i = i + 1, # update timestep
                                  dbh = dbh + growth * (1 - ci), # add increase to current DBH
                                  type = dplyr::case_when(dbh <= 10 ~ "Sapling", # update type
                                                          dbh > 10 ~ "Adult"))

  # combine tibbles
  full_updated <- dplyr::bind_rows(current_living, input_unnested)

  # nest tibble
  result <- tidyr::nest(full_updated, -c(id, x, y, species), .key = "data")

  return(result)
}
