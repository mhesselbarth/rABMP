#' deprecated_simulate_seed_dispersal
#'
#' @description Simulate seed dispersal
#'
#' @param input Tibble with input data
#' @param threshold Minimum DBH threshold for reproduction
#'
#' @details
#' Simulates seed dispersal by first calculating the number of seeds for each tree
#' and following distributing them around parental trees following a seed kernel.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' names(example_input_data)
#' df_trees <- prepare_input(input = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' deprecated_simulate_seed_dispersal(df_trees)
#' }
#'
#' @aliases deprecated_simulate_seed_dispersal
#' @rdname deprecated_simulate_seed_dispersal
#'
#' @references
#' Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed
#' dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475-1494.
#'
#' Bilek 2009 Paper
#'
#' Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L.,
#' Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed
#' dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531-1545.
#'
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' @export
deprecated_simulate_seed_dispersal <- function(input, threshold = 30){

  # unnest data
  input <- tidyr::unnest(input)

  # get most recent time step
  max_i <- max(input$i)

  # only get living trees of current timestep above threshold
  current_living <- input[which(input$type != "Dead" &
                                  input$i == max_i &
                                  input$dbh > threshold), ]

  # Number of seedlings for each tree
  number_seedlings <- calculate_seeds(species = current_living$species,
                                      dbh = current_living$dbh)

  number_seedlings <- floor(number_seedlings * stats::runif(n = 1, min = 0.812, max = 0.83) * 0.0236)

  # which trees produce surviving seedlings?
  id <- which(number_seedlings > 0)

  # only number seedlings > 0
  number_seedlings <- number_seedlings[id]

  species <- current_living$species[id]

  # calculate seedlings coordinates
  seedlings <- deprecated_rcpp_create_seedlings(coords = as.matrix(current_living[id, 2:3]),
                                                number = number_seedlings,
                                                species = species)

  # create seedlings id
  id <- seq(from = max(input$id) + 1, to = max(input$id) + nrow(seedlings))

  # create tibble
  seedlings <- tibble::tibble(id = id,
                              x = seedlings[, 1],
                              y = seedlings[, 2],
                              species = rep(x = species, times = number_seedlings),
                              i = max_i,
                              type = "Seedling",
                              dbh = 1.0,
                              ci = 0.0)

  # combine to one data frame
  result <- rbind(input, seedlings)

  # nest dataframe
  result <- tidyr::nest(result, -c(id, x, y, species), .key="data")

  return(result)
}
