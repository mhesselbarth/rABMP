#' simulate_seed_dispersal
#'
#' @details
#' Simulate seed dispersal
#'
#' @param input Tibble with input data
#' @param threshold Minimum DBH threshold for reproduction
#'
#' @references \itemize{
#' \item Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475–1494.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#'
#' @export
simulate_seed_dispersal <- function(input, threshold = 30){

  # unnest data
  input_unnested <- tidyr::unnest(input)

  # only get living trees of current timestep above threshold
  current_living <- dplyr::filter(input_unnested,
                                  type != "Dead",
                                  i == max(i),
                                  dbh > threshold)

  # get most recent time step
  i <- max(current_living$i)

  # Number of seedlings for each tree
  no_seedlings <- purrr::map2_dbl(current_living$species, current_living$dbh,
                                  function(x, y) rABMP::number_seeds(species = x,
                                                                     dbh = y))

  # Reduce seedlings because of browsing and general mortality
  no_seedlings <- floor(no_seedlings * 0.3) # parameter needs update, just random number right now

  # Create seedlings
  seedlings <- purrr::pmap_dfr(list(current_living$species, no_seedlings, current_living$x, current_living$y),
                                      function(species, n, x_coord, y_coord) {
                                        distance_x <- rABMP::random_distance(species = species, n = n)
                                        coords_x <- x_coord + distance_x

                                        distance_y <- rABMP::random_distance(species = species, n = n)
                                        coords_y <- y_coord + distance_y

                                        tibble::tibble(x = coords_x,
                                                       y = coords_y,
                                                       species = species,
                                                       i = i,
                                                       type = "Seedling",
                                                       dbh = 1.0,
                                                       ci = 0.0)
                                        })


  result <- dplyr::bind_rows(input_unnested, seedlings)

  result <- tidyr::nest(result, -c(id, x, y, species), .key="data")

  return(result)
}




