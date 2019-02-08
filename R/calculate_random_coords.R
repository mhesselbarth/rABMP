#' random_distance
#'
#' @details
#' Create distances from origin depending on seed kernel function
#' @param species Species of tree
#' @param n Number of random coordinates
#' @param max_dist Maximum distance of seed dispersal
#' @param number_samples Starting number of random coordinates checked against the seed kernal function.
#' A higher number will decrease the speed of the function but increase the fit to the seed kernel
#'
#' @export
calculate_random_coords <- function(species, n = NULL, max_dist = 80, number_samples = 1000000){

  # random numbers between 0 and max dispersal distance
  proposed_coords <- tibble::tibble(proposed = runif(number_samples,
                                                     min = 0, max = max_dist))

  # Create coordinates according to kernel
  proposed_coords <- dplyr::mutate(proposed_coords,
                                   kernel = rabmp::calculate_seed_kernel(species = species,
                                                                         distance = proposed,
                                                                         max_dist = max_dist), # probability of random number according to seed dipersal
                                   target = runif(number_samples, min = 0, max = 1), # test value
                                   accept = dplyr::case_when(target <= kernel / max(kernel, na.rm = TRUE) ~ TRUE, # set TRUE for numbers that fit distribution
                                                             TRUE ~ FALSE))

  coords_filterd <- dplyr::filter(proposed_coords, accept == TRUE) # only coordinates that fit the distribution

  # Distance in both directions
  coords <- c(dplyr::pull(coords_filterd, proposed),
              dplyr::pull(coords_filterd, proposed) * -1)

  if(is.null(n)){n <- length(result)} # no provided, returns all coordinates

  result <- sample(coords, n) # only n coordinates

  return(result)
}

