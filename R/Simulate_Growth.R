#' Update DBH
#'
#' The function simulates growth of individual points
#' @param input [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' @param year [\code{Numeric(1)}]\cr Year of simulation
#' @return Vector with size increase for each point
#' @examples
#'
#' @export
Simulate.Growth <- function(input, year){

  result <- input %>%
    dplyr::mutate(!!paste0("Year_", year-1) := DBH,
                  DBH = dplyr::case_when(Type == "Dead" ~ DBH,
                                         Type == "Alive" ~
                                           DBH + purrr::pmap_dbl(., function(Species, DBH, CI,...){
                                             Growth.Function.Species(dbh=DBH, species=Species) * (1-CI)})))

  return(result)
}



