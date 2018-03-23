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
    rABMP::Number.Seeds(species = Species, dbh = DBH) %>%
        magrittr::multiply_by(0.05) %>%
        floor()}))

  current_i <- living %>%
    dplyr::select(i) %>%
    max()

  random_coords <- living %>%
    dplyr::select(Species) %>%
    unique() %>%
    dplyr::mutate(Random_coords = purrr::map(., rABMP::Random.Coordinates(Species))) # Error

    seedlings <- living %>%
      purrr::pmap_dfr(., function(ID, x, y, Species, No_seedlings, ...){
        tibble::tibble(x = x + sample(unlist(dplyr::filter(random_coords, species==Species)[[2]]), No_seedlings),
                       y = y + sample(unlist(dplyr::filter(random_coords, species==Species)[[2]]), No_seedlings),
                       Species = Species,
                       i = current_i,
                       Type = "Seedling",
                       DBH = 1.0, CI = 0)})

  result <- input %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(seedlings) %>%
    tidyr::nest(-c(x,y, Species), .key="Data")

  return(result)
}




