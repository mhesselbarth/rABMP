#' Random Coordinates
#'
#' Create Random coordinates depending on seed kernel function
#'
#' @param species Species of tree
#' @param n Number of random coordinates
#' @param max_dist Maximum distance of seed dispersal
#' @param number_samples Starting number of random coordinates checked against the seed kernal function.
#' A higher number will decrease the speed of the function but increase the fit to the seed kernel
#'
#' @export
random_coordinates <- function(species, n = NULL, max_dist = 80, number_samples = 1000000){

  result <- tibble::tibble(Proposed = runif(number_samples, min = 0, max = max_dist)) %>% # random numbers between 0 and max dispersal distance
    dplyr::mutate(Target = seed_kernel(species = species, distance = Proposed, max_dist = max_dist), # probability of random number according to seed dipersal
                  Random = runif(number_samples, min = 0, max = 1), # test value
                  Accept = dplyr::case_when(Random <= Target / max(Target, na.rm = TRUE) ~ TRUE, # set TRUE for numbers that fit distribution
                                          TRUE ~ FALSE)) %>%
    dplyr::filter(Accept == TRUE) %>% # only coordinates that fit the distribution
    dplyr::pull(Proposed) %>%
    c(. , . * -1) # coordinates in both directions

  if(is.null(n)){n <- length(result)} # no provided, returns all coordinates

  result <- result %>% # only n coordinates
    sample(n)

  return(result)
}

