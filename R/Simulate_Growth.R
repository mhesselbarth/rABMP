#' simulate_growth
#'
#' @description Simulate growth
#'
#' @param input input dataframe
#'
#' @details
#' Simulate growth by calculating the DBH increase and add it to the current DBH.
#' Also, i (iteration counter) will be increase and the type re-classified based
#' on the DBH: <ADD CLASSIFICATION SCHEME>
#'
#' @return vector
#'
#' @examples
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @references
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' @export
simulate_growth <- function(input){

  # unnest data
  input <- tidyr::unnest(input)

  # only get living trees of current timestep
  current_living <- input[which(input$type != "Dead" & input$i == max(input$i)), ]

  # calculate growth
  growth <- calculate_growth(dbh = current_living$dbh)

  # for exponential type
  v <- 3.33278

  # update DBH reduced by ci
  current_living$dbh <- current_living$dbh + growth * v *(1 - current_living$ci)

  # update type below dbh <= 10 cm
  current_living$type[which(current_living$dbh <=10)] <- "Sapling"

  # update type below dbh > 10 cm
  current_living$type[which(current_living$dbh >10)] <- "Adult"

  # combine tibbles
  input <- rbind(current_living, input)

  # nest tibble
  input <- tidyr::nest(input, -c(id, x, y, species), .key = "data")

  return(input)
}
