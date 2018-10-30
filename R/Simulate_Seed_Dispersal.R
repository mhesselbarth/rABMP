#' simulate_seed_dispersal
#'
#' @details
#' Simulate seed dispersal
#'
#' @param input  Tibble with input data
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

  # Number of seedlings for each tree
  no_seedlings <- purrr::map2_dbl(current_living$species, current_living$dbh,
                                  function(x, y) number_seeds(species = x,
                                                              dbh = y))

  # Here it gets tricky...need to add random coords to starting coord and return tibble
  purrr::map2(current_living$species, no_seedlings,
              function(x, y) random_coordinates(species = x,
                                                n = y))


  # tibble::tibble(x = x + rABMP::random_coordinates(species = Species, n = number_seedlings),
  #                y = y + rABMP::random_coordinates(species = Species, n = number_seedlings),
  #                Species = Species,
  #                i = i,
  #                Type = 'Seedling',
  #                DBH = 1.0,
  #                CI = 0.0)

    # seedlings <- living %>%
    #   purrr::pmap_dfr(., function(x, y, Species, i, No_seedlings, ...){
    #     tibble::tibble(x = x + sample_n(dplyr::filter(tidyr::unnest(random_coords), Species == Species), size = 1)$Coordinates,
    #                    y = y + sample_n(dplyr::filter(tidyr::unnest(random_coords
    #                                                                 ), Species == Species), size = 1)$Coordinates,
    #                    Species = Species,
    #                    i = i,
    #                    Type = 'Seedling',
    #                    DBH = 1.0,
    #                    CI = 0.0)})



  # result <- input %>%
  #   tidyr::unnest() %>%
  #   dplyr::bind_rows(seedlings) %>%
  #   tidyr::nest(-c(x,y, Species), .key="Data")

  return(result)
}




