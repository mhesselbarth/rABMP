#' calculate_number_seeds
#'
#' @description Calculate number of seeds
#'
#' @param species String with current species.
#' @param dbh Numeric with DBH of current tree.
#' @param parameters List with all parameters. See details for more information.
#'
#' @details
#' Calculates the number of produced seeds for each tree (without reduction for
#' seed mortality).
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' calculate_number_seeds(species = "Beech", dbh = 45.25)
#' }
#'
#' @aliases calculate_number_seeds
#' @rdname calculate_number_seeds
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' @export
deprecated_calculate_number_seeds <- function(species, dbh, parameters){

  number_seeds <- ifelse(test = species == "beech",
                         yes = parameters$seed_str_beech * ((dbh / 30) ^ 2),
                         no = ifelse(test = species == "ash",
                                     yes  = parameters$seed_str_ash * ((dbh / 30) ^ 2),
                                     no = ifelse(test = species == "sycamore",
                                                 yes = parameters$seed_str_sycamore * ((dbh / 30) ^ 2),
                                                 no = ifelse(test = species == "hornbeam",
                                                             yes = parameters$seed_str_hornbeam * ((dbh / 30) ^ 2),
                                                             no = ifelse(test = species == "others",
                                                                         yes = parameters$seed_str_others * ((dbh / 30) ^ 2),
                                                                         no = NA)))))

  if (anyNA(number_seeds)) {
    warning("NAs introduced by calculate_number_seeds()")
  }

  return(number_seeds)
}
