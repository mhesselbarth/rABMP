#' Seed dispersal (not running)
#'
#' The function simulates seed dispersal
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @param threshold [\code{numeric(1)}]\cr Minimum DBH threshold for reproduction
#' @references \itemize{
#' \item Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475–1494.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#'
#' @export
Simulate.Seed.Dispersal <- function(input, threshold = 30){

  living <- input %>%
    tidyr::unnest() %>%
    dplyr::filter(Type == "Adult" & i == max(i) & DBH > threshold) %>%
    dplyr::mutate(No_seedlings = purrr::pmap_dbl(., function(Species, DBH, ...){
    rABMP::Number.Seeds(species = Species, dbh = DBH)}))

  # current_i <- living %>%
  #   dplyr::select(i) %>%
  #   max()

  random_coords <- living %>%
    dplyr::select(Species) %>%
    unique() %>%
    purrr::pmap_dfr(., function(Species){tibble(Coordinates = rABMP::Random.Coordinates(species = Species),
                                                Species = Species)}) %>%
    tidyr::nest(-Species)


  system.time(seedlings <- living %>%
    purrr::pmap_dfr(function(x, y, Species, i, DBH, ...){
      number_seedlings <- rABMP::Number.Seeds(species = Species, dbh = DBH)

      tibble::tibble(x = x + rABMP::Random.Coordinates(species = Species, n = number_seedlings),
                     y = y + rABMP::Random.Coordinates(species = Species, n = number_seedlings),
                     Species = Species,
                     i = i,
                     Type = 'Seedling',
                     DBH = 1.0,
                     CI = 0.0)
    }))

    seedlings <- living %>%
      purrr::pmap_dfr(., function(x, y, Species, i, No_seedlings, ...){
        tibble::tibble(x = x + sample_n(dplyr::filter(tidyr::unnest(random_coords), Species == Species), size = 1)$Coordinates,
                       y = y + sample_n(dplyr::filter(tidyr::unnest(random_coords
                                                                    ), Species == Species), size = 1)$Coordinates,
                       Species = Species,
                       i = i,
                       Type = 'Seedling',
                       DBH = 1.0,
                       CI = 0.0)})

        # tibble::tibble(x = x + sample(tidyr::unnest(dplyr::filter(random_coords, species==Species)[[2]]), No_seedlings),
        #                y = y + sample(tidyr::unnest(dplyr::filter(random_coords, species==Species)[[2]]), No_seedlings),
        #                Species = Species,
        #                i = current_i,
        #                Type = "Seedling",
        #                DBH = 1.0, CI = 0)})

  result <- input %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(seedlings) %>%
    tidyr::nest(-c(x,y, Species), .key="Data")

  return(result)
}




