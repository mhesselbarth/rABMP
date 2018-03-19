#' Seed dispersal
#'
#' The function simulates growth of individual points
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @param dbh [\code{String(1)}]\cr Name of the DBH mark
#' @references \itemize{
#' \item Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475–1494.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#' @examples
#' CI <- Seed.dispersal(pattern=pattern_living_trees)
#'
#' @export
Simulate.Seed.Dispersal <- function(input, threshold=30){

  seedlings <- input %>%
    dplyr::filter(Type=="Alive" & DBH>=threshold) %>%
    dplyr::mutate(No_seedlings=purrr::pmap_dbl(., function(Species, DBH, ...){
      Number.Seeds(species=Species, dbh=DBH) %>%
        floor() %>%
        magrittr::multiply_by(0.05)
        })) %>%
    purrr::pmap_dfr(., function(x, y, Species, No_seedlings, ...){
      tibble(x = x + Random.Coordinates(species = Species, n = No_seedlings),
             y = y + Random.Coordinates(species = Species, n = No_seedlings),
             Species = Species,
             Type="Seedling",
             DBH=1.0, CI=0)
    })

  result <- dplyr::bind_rows(input, seedlings)

  return(result)
}




