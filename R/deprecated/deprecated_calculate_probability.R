#' #' deprecated_calculate_probability
#' #'
#' #' @description Calculate kernel probability
#' #'
#' #' @param beta parameter dispersal kernel
#' #' @param theta parameter dispersal kernel
#' #' @param max_dist maximum interaction distance
#' #'
#' #' @details
#' #' Calculates the probability according a dispersal kernal.
#' #'
#' #' @return vector
#' #'
#' #' @aliases deprecated_calculate_probability
#' #' @rdname deprecated_calculate_probability
#' #'
#' #' @references
#' #' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' #' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#' #'
#' #' @export
#' deprecated_calculate_probability <- function(beta, theta, max_dist) {
#'
#'   probability_temp <- 0
#'
#'   probability <- rep(NA, max_dist)
#'
#'   # normalizer <- integrate(function(r) {pi * (2 * (r + 0.5)) * exp(-beta * r ^ theta)}, lower = 0, upper = Inf)$value
#'   normalizer <- integrate(function(r) {exp(-beta * r ^ theta)}, lower = 0, upper = Inf)$value
#'
#'   normalizer <- ifelse(test = normalizer < 0.0001,
#'                        yes = 0, no = 1.0 / normalizer)
#'
#'   for(distance in 1:max_dist) {
#'
#'     # probability_temp <- probability_temp + normalizer * pi * (2 * (distance + 0.5)) * exp(-beta * distance ^ theta)
#'     probability_temp <- probability_temp + normalizer * exp(-beta * distance ^ theta)
#'
#'     probability[[distance]] <- probability_temp
#'   }
#'
#'   # prob can't be larger than 1
#'   probability[which(probability > 1)] <- 1
#'
#'   # last value in vector should be 1
#'   probability[length(probability)] <- 1
#'
#'   return(probability)
#' }
