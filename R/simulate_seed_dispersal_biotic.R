#' simulate_seed_dispersal_biotic
#'
#' @description Simulate seed dispersal
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
#' @param plot_area The plot area as \code{\link{owin}} object from the \code{spatstat} package.

#'
#' @details
#' Simulates seed dispersal by first calculating the number of seeds for each tree
#' and following distributing them around parental trees following a seed kernel.
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#'  type = "Class", dbh = "bhd")
#'
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")
#'
#' simulate_seed_dispersal_biotic(df_trees, parameters = parameters)
#' }
#'
#' @aliases simulate_seed_dispersal_biotic
#' @rdname simulate_seed_dispersal_biotic
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
# Olesen, C.R., Madsen, P., 2008. The impact of roe deer (Capreolus capreolus),
# seedbed, light and seed fall on natural beech (Fagus sylvatica) regeneration.
# For. Ecol. Manag. 255, 3962–3972.
#'
#' @export
simulate_seed_dispersal_biotic <- function(data, parameters, plot_area){

  # get id of current living
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # number of seedlings for each tree (Ribbens et al. 1994 formula 1)
  number_seedlings <- rcpp_calculate_number_seeds(dbh = data[id, dbh],
                                                  str = parameters$seed_str)

  # reduce seedlings
  number_seedlings <- round(number_seedlings, digits = 0)

  # id of seedlings > 0
  id_seedlings <- which(number_seedlings > 0)

  # create seedlings
  if (length(id_seedlings) != 0) {

    # id of trees that produced seedlings
    id <- id[id_seedlings]

    # only number of seedlings that are large than 0
    number_seedlings <- number_seedlings[id_seedlings]

    # calculate seedlings coordinates (Ribbens et al. 1994 formula 2)
    seedlings <- rcpp_create_seedlings(coords = as.matrix(data[id, .(x, y)]),
                                       number =  number_seedlings,
                                       beta = parameters$seed_beta,
                                       max_dist = parameters$seed_max_dist)

    # remove seedlings not inside plot
    seedlings <- seedlings[spatstat::inside.owin(x = seedlings[, 1],
                                                 y = seedlings[, 2],
                                                 w = plot_area), ]

    # get random threshold
    random_thres <- runif(n = nrow(seedlings), min = 0, max = 1)

    # which seedlings should be kept
    include_id <- which(random_thres < parameters$seed_success, arr.ind = TRUE)

    # reduce seedlings
    seedlings <- seedlings[include_id, ]

    # create data.table
    # create seedlings id larger than existing max id
    # create random dbh
    seedlings <- data.table::data.table(id = seq(from = max(data$id) + 1,
                                                 to = max(data$id) + nrow(seedlings),
                                                 by = 1),
                                        i = max(data$i),
                                        x = seedlings[, 1],
                                        y = seedlings[, 2], type = "seedling",
                                        dbh = stats::runif(n = nrow(seedlings),
                                                           min = 0.5, max = 1),
                                        ci = 0.0)

    # combine to one data frame with all data
    data <- rbind(data, seedlings)
  }

  return(data)
}
