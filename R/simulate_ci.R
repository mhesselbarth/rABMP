#' simulate_ci
#'
#' @description Update competition index
#'
#' @param input input dataframe
#' @param max_dist maximum interaction distance between trees
#'
#' @details
#' The function calculated a compeition index using a kernel. Competition depends on
#' neighbouring trees (within max_dist). If \code{standardized = TRUE}, all values
#' are standardized to 0 <= CI <= 1.
#'
#' @return tibble
#'
#' @examples
#' names(example_input_data)
#' df_tress <- prepare_input(input = example_input_data, x = "x_coord", y = "y_coord",
#' species = "spec", type = "Class", dbh = "bhd")
#' simulate_ci(input = df_tress)
#'
#' @aliases simulate_ci
#' @rdname simulate_ci
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
simulate_ci <- function(input, max_dist = 30){

  # unnest data
  input <- tidyr::unnest(input)

  # data of past time steps
  past <- input[which(input$i != max(input$i)), ]

  # data of current time step
  current <- input[which(input$i == max(input$i)), ]

  # calculate CI
  competition <- rcpp_calculate_ci(matrix = as.matrix(current[, c(2, 3, 7)]),
                                   alpha = 1.45772,
                                   beta = 0.52339,
                                   max_dist = max_dist)

  # transformation of competition index which includes size of focal tree
  # scaled between 0 and 1
  alpha <- 1.45772
  competition <- competition / (current$dbh ^ alpha + competition)

  # update tibble
  current$ci <- competition

  # combine tibbles
  input <- rbind(current, past)

  # nest tibble
  input <- tidyr::nest(input, -c(id, x, y, species), .key="data")

  return(input)
}
