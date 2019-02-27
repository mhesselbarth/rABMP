#' deprecated_calculate_random_distance
#'
#' @description Calculate random coords
#'
#' @param species Current species
#' @param n number of distances
#' @param max_dist maximum distance
#' @param number_samples random number used to generate distances
#'
#' @details
#' Calculates random distances following the probability function of a species-specific
#' seed kernel. Because a 'brute-force' approach is used, computational demanding
#'
#' @return vector
#'
#' @examples
#' deprecated_calculate_random_distance(species = "Beech", n = 10)
#'
#' @aliases deprecated_calculate_random_distance
#' @rdname deprecated_calculate_random_distance
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' @export
deprecated_calculate_random_distance <- function(species, n = NULL, max_dist = 80, number_samples = 1000000) {

  # random numbers between 0 and max dispersal distance
  proposed_coords <- tibble::tibble(proposed = runif(number_samples,
                                                     min = 0, max = max_dist))

  # Create coordinates according to kernel
  proposed_coords <- dplyr::mutate(proposed_coords,
                                   kernel = deprecated_calculate_seed_kernel(species = species,distance = proposed,
                                                                             max_dist = max_dist), # probability of random number according to seed dipersal
                                   target = stats::runif(number_samples, min = 0, max = 1), # test value
                                   accept = dplyr::case_when(target <= kernel / max(kernel, na.rm = TRUE) ~ TRUE, # set TRUE for numbers that fit distribution
                                                             TRUE ~ FALSE))
#
  coords_filterd <- dplyr::filter(proposed_coords, accept == TRUE) # only coordinates that fit the distribution

  # Distance in both directions
  coords <- c(dplyr::pull(coords_filterd, proposed),
              dplyr::pull(coords_filterd, proposed) * -1)

  if(is.null(n)){n <- length(result)} # no provided, returns all coordinates

  result <- sample(coords, n) # only n coordinates

  return(result)
}

