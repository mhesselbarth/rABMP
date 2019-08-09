#' simulate_seed_dispersal
#'
#' @description Simulate seed dispersal
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
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
simulate_seed_dispersal <- function(data, parameters){

  # unnest data
  data <- tidyr::unnest(data)

  # data of past time steps
  # past <- data[which(data$i != max(data$i)), ]

  # data of current time step
  current <- data[which(data$type != "Dead" & data$i == max(data$i)), ]

  # Number of seedlings for each tree (Ribbens et al. 1994 formula 1)
  number_seedlings <- rcpp_calculate_number_seeds(species = current$species,
                                                  dbh = current$dbh,
                                                  str_beech = parameters$seed_str_beech,
                                                  str_ash = parameters$seed_str_ash,
                                                  str_sycamore = parameters$seed_str_sycamore,
                                                  str_hornbeam = parameters$seed_str_hornbeam,
                                                  str_others = parameters$seed_str_others)

  # reduce seedlings (Bilek et al. 2009 p150)
  number_seedlings <- floor(number_seedlings *
                              parameters$seed_empty * parameters$seed_success)

  # which trees produce surviving seedlings?
  id <- which(number_seedlings > 0)

  # only number seedlings > 0
  number_seedlings <- number_seedlings[id]

  # species of trees that produced seedlings
  species <- current$species[id]

  # calculate seedlings coordinates (Ribbens et al. 1994 formula 2)
  seedlings <- rcpp_create_seedlings(coords = as.matrix(current[id, 2:3]),
                                     number = number_seedlings,
                                     species = species)

  # create seedlings id
  id <- seq(from = max(data$id) + 1, to = max(data$id) + nrow(seedlings))

  # create random dbh
  random_dbh <- stats::runif(n = length(id), min = 0.1, max = 1)

  # create tibble
  seedlings <- tibble::tibble(id = id,
                              x = seedlings[, 1],
                              y = seedlings[, 2],
                              species = rep(x = species, times = number_seedlings),
                              i = max(current$i),
                              type = "Seedling",
                              dbh = random_dbh,
                              ci = 0.0)

  # combine to one data frame
  data <- rbind(seedlings, data)

  # nest dataframe
  data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
