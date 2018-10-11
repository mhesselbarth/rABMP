#' Update competition index
#'
#' The function updates the competition index of the provided pattern a kernel.
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @param standardized [\code{logical(1)}]\cr Standardize maximum CI to 1
#' @param max_dist [\code{numeric(1)}]\cr Maximum interaction distance between trees
#' @references \itemize{
#' \item Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666–678.
#' \item Pommerening, A., Maleki, K., 2014. Differences between competition kernels and traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135–143.
#' }
#'
#' @export
update_competition_index <- function(input, standardized = T, max_dist = 30){

  past <- input %>% # data of previous time steps
    tidyr::unnest() %>%
    dplyr::filter(i != max(i))

  current <- input %>% # data of current time steps
    tidyr::unnest() %>%
    dplyr::filter(i == max(i))

  competition <- current %>%
   dplyr::select(x,y) %>% # get distances between all individuals
   dist() %>% # distance matrix
   as.matrix() %>%
   tibble::as.tibble() %>%
   purrr::map_dbl(rABMP::Calculate.Competition.Index, i = seq_along(.), dbh = current$DBH, max_dist = max_dist) %>% # calculate CI for each tree
   as.double()

  if(standardized == T){
    competition <- competition / max(competition) # standarize results
  }

  result <- current %>%
    dplyr::mutate(CI = competition) %>% # update CI
    dplyr::bind_rows(past) %>% # combine with data of previous time steps
    tidyr::nest(-c(x, y, Species), .key="Data")

  return(result)
}

