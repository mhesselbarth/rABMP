#' update_ci
#'
#' @description Update competition index
#'
#' @param input input dataframe
#' @param type kernel type (either "fractional", "exponential" or "epanechnikov")
#' @param max_dist maximum interaction distance between trees
#' @param standardized standardize maximum CI to 1
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
#' df_tress <- prepare_input(input = example_input_data, x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#' update_ci(input = df_tress)
#'
#' @aliases update_ci
#' @rdname update_ci
#'
#' @references
#' Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence
#' of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666–678.
#'
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135–143.
#'
#' @export
update_ci <- function(input,
                      type = "exponential",
                      max_dist = 30,
                      standardized = TRUE){

  # unnest data
  input <- tidyr::unnest(input)

  # data of past time steps
  past <- input[which(input$i != max(input$i)), ]

  # data of current time step
  current <- input[which(input$i == max(input$i)), ]

    # get coordinates
  coordinates <- matrix(c(current$x, current$y), ncol = 2)

  # get number of trees in current data
  number_tress <- nrow(current)

  # initialise vector for competition
  competition <- rep(x = NA, times = number_tress)

  # loop through all points
  # maybe use Rcpp?
  for(i in 1:number_tress) {

    # calculate distance between current point and all other points
    distance <- get_distance(point_a = coordinates[i ,, drop = FALSE],
                             point_b = coordinates)

    # only distances below threshold and not to itself
    dbh <- current$dbh[which(distance < max_dist & distance != 0)]

    distance <- distance[which(distance < max_dist & distance != 0)]

    # calculate competition of current tree
    competition[i] <- sum(calculate_ci(distance = distance,
                                       dbh = dbh,
                                       max_dist = max_dist,
                                       type = type))
  }

  # pommerening 2014: transformation of competition index which includes size of affected tree
  # scaled between 0 and 1
  # MH: Do we neeed to remove the "scaling" by DBH from calculate_ci?
  alpha <- 1.45772
  competition <- competition / (current$dbh ^ alpha + competition)

  # standarize results to max(competition) = 1
  # MH: Do we still need this?
  if(standardized == TRUE){
    competition <- competition / max(competition)
  }

  # update tibble
  current$ci <- competition

  # combine tibbles
  input <- dplyr::bind_rows(current, past)

  # nest tibble
  input <- tidyr::nest(input, -c(id, x, y, species), .key="data")

  return(input)
}
