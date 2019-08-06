#' simulate_ci
#'
#' @description Update competition index
#'
#' @param data Dataframe with input data.
#' @param max_dist Numeric with maximum interaction distance between trees.
#' @param parameters List with all parameters.
#'
#' @details
#' The function calculated a compeition index using a kernel. Competition depends on
#' neighbouring trees (within max_dist). If \code{standardized = TRUE}, all values
#' are standardized to 0 <= CI <= 1.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' names(example_input_data)
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' parameters <- construct_parameters()
#' simulate_ci(data = df_trees, parameters = parameters)
#' }
#'
#' @aliases simulate_ci
#' @rdname simulate_ci
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
simulate_ci <- function(data, parameters){

  # unnest data
  data <- tidyr::unnest(data)

  # data of past time steps
  past <- data[which(data$i != max(data$i)), ]

  # data of current time step
  current <- data[which(data$i == max(data$i)), ]

  # calculate CI
  competition <- rcpp_calculate_ci(matrix = as.matrix(current[, c(2, 3, 7)]),
                                   alpha = parameters$alpha,
                                   beta = parameters$beta,
                                   max_dist = parameters$max_dist)

  # transformation of competition index which includes size of focal tree
  # scaled between 0 and 1
  competition <- competition / (current$dbh ^ parameters$alpha + competition)

  # update tibble
  current$ci <- competition

  # combine tibbles
  data <- rbind(current, past)

  # nest tibble
  data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")

  return(data)
}
