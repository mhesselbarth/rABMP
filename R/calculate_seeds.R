#' calculate_seeds
#'
#' @description Calculate number of seeds
#'
#' @param species current species
#' @param dbh dbh of current tree
#'
#' @details
#' Calculates the number of produced seeds for each tree (without reduction for
#' seed mortality).
#'
#' @return vector
#'
#' @examples
#' calculate_seeds(species = "Beech", dbh = 45.25)
#'
#' @aliases calculate_seeds
#' @rdname calculate_seeds
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' @export
calculate_seeds <- function(species, dbh){

  number_seeds <- ifelse(test = species == "Beech",
                         yes = 121.22 * (dbh / 30) ^ 2,
                         no = ifelse(test = species == "Ash",
                                     yes  = 26.18 * (dbh / 30) ^ 2,
                                     no = ifelse(test = species == "Sycamore",
                                                 yes = no_seeds <- 182.42 * (dbh/30) ^ 2,
                                                 no = ifelse(test = species == "Hornbeam",
                                                             yes = no_seeds <- 121.22 * (dbh/30) ^ 2,
                                                             no = ifelse(test = species == "others",
                                                                         yes = 112.76 * (dbh / 30) ^ 2,
                                                                         no = NA)))))

  if(anyNA(number_seeds)) {
    warning("NAs introduced by calculate_seeds()")
  }

  return(number_seeds)
}
