#' simulate_mortality
#'
#' @details
#' Function to model mortality of trees
#'
#' @param input Tibble with input data
#'
#' @references \itemize{
#' \item Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230.
#' }
#'
#' @export
simulate_mortality <- function(input) {

  # unnest data
  input <- tidyr::unnest(input)

  # only get living trees of current timestep
  current_living <- input[which(input$type != "Dead" & input$i == max(input$i)), ]

  # calculate mortality prob
  mortality_prob <- rcpp_calculate_mortality_probs(species = current_living$species,
                                                   dbh = current_living$dbh)

  # create random number for all living trees
  random_number <- stats::runif(n = length(mortality_prob), min = 0, max = 1)

  # set all to dead if mortality prob is larger than random number
  # MH: Is this actually what we want? Does it make a difference to random_number > mortality_prob?
  current_living$type[which(random_number < mortality_prob)] <- "Dead"

  # combine tibbles
  # MH: Old data missing?
  input <- rbind(current_living, input[which(input$i != max(input$i)), ])

  # nest tibble
  input <- tidyr::nest(input, -c(id, x, y, species), .key = "data")

  return(input)
}
