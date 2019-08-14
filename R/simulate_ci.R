#' simulate_ci
#'
#' @description Update competition index
#'
#' @param data Dataframe with input data.
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
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = "\t")
#'
#' names(example_input_data)
#' df_trees <- prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#'
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

  # data of current time step
  id <- which(data$type != "dead" & data$i == max(data$i))

  # calculate CI (Pommerening et al. 2014 formula 6)
  # transformation of ci, which includes size of focal tree (Pommerening et al. 2014 formula 9)
  # scaled between 0 and 1
  competition <- rcpp_calculate_ci(matrix = as.matrix(data[id, c("x", "y", "dbh")]),
                                   alpha = parameters$ci_alpha,
                                   beta = parameters$ci_beta,
                                   max_dist = parameters$ci_max_dist)

  # update tibble
  data$ci[id] <- competition

  return(data)
}
