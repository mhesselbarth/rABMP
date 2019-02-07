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
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135–143.
#'
#' @export
simulate_growth <- function(input){

  # unnest data
  input <- tidyr::unnest(input)

  # only get living trees of current timestep
  current_living <- input[which(input$type != "Dead" & input$i == max(input$i)), ]

  # calculate (potential) growth
  # growth <- purrr::map_dbl(current_living$dbh,
  #                          function(x) rabmp::growth_function_species(dbh = x))
  # growth <- apply(current_living$dbh, 1,  FUN = growth_function_species(dbh = x))

  growth <- calculate_growth(dbh = current_living$dbh)

  # current_living$pot.growth <- growth
  # v <- 3.33278
  # current_living$growth_inc <- current_living$pot.growth * v * (1-current_living$ci)
  # plot(current_living$dbh, current_living$pot.growth, xlab="dbh", ylab="dbh increment", col="darkred", ylim=c(0,1.5))
  # points(current_living$dbh, current_living$growth_inc)
  # legend("topright", legend=c("potential growth", "actual growth"), col=c("darkred", "black"),pch=1)
  # current_living$higher <- current_living$growth_inc >  current_living$pot.growth
  # summary(current_living$higher)
  # plot(y[higher=="FALSE"] ~ x[higher=="FALSE"], data=current_living)
  # points(y[higher=="TRUE"] ~ x[higher=="TRUE"], data=current_living, col="red")

  # mabye put the growth * v *(1 - ci) part in line 21/22 to have all in the same place
  v <- 3.33278 # for exponential type

  # update tibble
  # MH: Is the type classification correct or is the seedling case missing?
  current_living <- dplyr::mutate(current_living,
                                  i = i + 1, # update timestep
                                  dbh = dbh + growth * v *(1 - ci), # add increase to current DBH
                                  type = dplyr::case_when(dbh <= 10 ~ "Sapling", # update type
                                                          dbh > 10 ~ "Adult"))

  # combine tibbles
  input <- dplyr::bind_rows(current_living, input)

  # nest tibble
  input <- tidyr::nest(input, -c(id, x, y, species), .key = "data")

  return(result)
}
