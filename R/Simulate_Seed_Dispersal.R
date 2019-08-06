#' simulate_seed_dispersal
#'
#' @description Simulate seed dispersal
#'
#' @param data Dataframe with input data.
#' @param threshold Numerich with minimum DBH threshold for reproduction.
#'
#' @details
#' Simulates seed dispersal by first calculating the number of seeds for each tree
#' and following distributing them around parental trees following a seed kernel.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' names(example_input_data)
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' simulate_seed_dispersal(df_trees)
#' }
#'
#' @aliases simulate_seed_dispersal
#' @rdname simulate_seed_dispersal
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' Bilek, L., Remes, J., Zahradnik, D., 2009. Natural regeneration of senescent even-
#' aged beech (Fagus sylvatica L.) stands under the conditions of Central Bohemua.
#' Journal of Forest Science 55(4), 145-155
#'
#' @export
simulate_seed_dispersal <- function(data, threshold = 30){

  # unnest data
  data <- tidyr::unnest(data)

  # get most recent time step
  max_i <- max(data$i)

  # only get living trees of current timestep above threshold
  current_living <- data[which(data$type != "Dead" &
                                 data$i == max_i &
                                 data$dbh > threshold), ]

  # Number of seedlings for each tree
  number_seedlings <- rabmp::calculate_seeds(species = current_living$species,
                                             dbh = current_living$dbh)

  # reduce seedlings according to Bilek et al. 2009
  number_seedlings <- floor(number_seedlings * stats::runif(n = 1, min = 0.812, max = 0.83) * 0.0236)

  # which trees produce surviving seedlings?
  id <- which(number_seedlings > 0)

  # only number seedlings > 0
  number_seedlings <- number_seedlings[id]

  species <- current_living$species[id]

  # calculate seedlings coordinates
  seedlings <- rcpp_create_seedlings(coords = as.matrix(current_living[id, 2:3]),
                                     number = number_seedlings,
                                     species = species)

  # create seedlings id
  id <- seq(from = max(data$id) + 1, to = max(data$id) + nrow(seedlings))

  # create random dbh
  random_dbh <- stats::runif(n = length(id), min = 0.2, max = 0.8)

  # create tibble
  seedlings <- tibble::tibble(id = id,
                              x = seedlings[, 1],
                              y = seedlings[, 2],
                              species = rep(x = species, times = number_seedlings),
                              i = max_i,
                              type = "Seedling",
                              dbh = random_dbh,
                              ci = 0.0)

  # combine to one data frame
  data <- rbind(data, seedlings)

  # nest dataframe
  data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
