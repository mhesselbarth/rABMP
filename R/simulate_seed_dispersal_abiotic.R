#' simulate_seed_dispersal_abiotic
#'
#' @description Simulate seed dispersal
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
#' @param abiotic RasterLayer with abiotic conditions. Should be scaled to 0 <= x <= 1.
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
#' df_trees <- prepare_data(data = example_input_data,
#' x = "x_coord", y = "y_coord", type = "Class", dbh = "bhd")
#'
#' threshold <- quantile(df_trees$dbh, probs = 0.8)
#'
#' plot_area <- spatstat::owin(xrange = c(0, 500), yrange = c(0, 500))
#'
#' ppp_threshold <- spatstat::ppp(x = df_trees[dbh > threshold, x],
#' y = df_trees[dbh > threshold, y],
#' window = plot_area)
#'
#' hetero <- spatstat::density.ppp(ppp_threshold,  dimyx = c(250, 250))
#' hetero_df <- tibble::as_tibble(hetero)
#' hetero_ras <- raster::rasterFromXYZ(hetero_df)
#'
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")
#'
#' simulate_seed_dispersal_abiotic(df_trees, parameters = parameters)
#' }
#'
#' @aliases simulate_seed_dispersal_abiotic
#' @rdname simulate_seed_dispersal_abiotic
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
simulate_seed_dispersal_abiotic <- function(data, parameters, plot_area,
                                            abiotic, abiotic_quantiles){

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

    # extract abiotic values
    abiotic_values <- rabmp::extract_abiotic(data = data.table::data.table(x = seedlings[, 1],
                                                                           y = seedlings[, 2]),
                                             abiotic = abiotic)

    if (anyNA(abiotic_values)) {

      stop("Some seedlings do not have an abiotic value related to them.",
           call. = FALSE)
    }

    # get probabilities for seed reduction
    probs <- ifelse(test = abiotic_values > abiotic_quantiles,
                    yes = parameters$seed_success_high,
                    no = parameters$seed_success)

    # get random threshold
    random_thres <- runif(n = nrow(seedlings), min = 0, max = 1)

    # which seedlings should be kept
    include_id <- which(random_thres < probs, arr.ind = TRUE)

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
                                        y = seedlings[, 2],
                                        type = "seedling",
                                        dbh = stats::runif(n = nrow(seedlings),
                                                           min = 0.5, max = 1),
                                        ci = 0.0,
                                        abiotic = abiotic_values[include_id])

    # combine to one data frame with all data
    data <- rbind(data, seedlings)
  }

  return(data)
}
