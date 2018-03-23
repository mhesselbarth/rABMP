#' Update DBH
#'
#' The function simulates growth of individual points
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @return Vector with size increase for each point
#'
#' @export
Simulate.Growth <- function(input){

  past <- input %>% # save the data of previous time step
    tidyr::unnest()

  result <- input %>%
    tidyr::unnest() %>%
    dplyr::filter(Type != "Dead" & i == max(i)) %>% # only living trees of the current time step
    dplyr::mutate(DBH = DBH + purrr::pmap_dbl(., function(Species, DBH, CI,...){
              rABMP::Growth.Function.Species(species=Species, dbh=DBH) * (1-CI)}), # add increase to current DBH
           i = i + 1, # update timestep
           Type = dplyr::case_when(DBH <= 10 ~ "Seedling", # update type
                     DBH > 10 ~ "Adult")) %>%
    dplyr::bind_rows(past) %>% # combine with data of of previous time step
    tidyr::nest(-c(x, y, Species), .key = "Data") %>%
    dplyr::mutate(Data=purrr::map(Data, function(x){dplyr::arrange(x, i)})) # order data in increasing i

  return(result)
}
