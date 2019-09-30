#' read_parameters
#'
#' @description Read parameters from text file
#'
#' @details
#' Construct a list with all default paramerts for run_model. The parameters include:
#' \itemize{
#'   \item First item
#'   \item Second item
#' }
#'
#' @param file String with path to text file.
#' @param return_list Logical if true parameters are returned as list.
#' @param ... Arguments passed on to \code{read.table}.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = "\t")
#' }
#'
#' @aliases read_parameters
#' @rdname read_parameters
#'
#' @references
#' Bilek, L., Remes, J., Zahradnik, D., 2009. Natural regeneration of senescent even-
#' aged beech (Fagus sylvatica L.) stands under the conditions of Central Bohemua.
#' Journal of Forest Science 55(4), 145-155
#'
#' Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die -
#' partitioning tree mortality dynamics in a near-natural mixed deciduous forest.
#' J. Ecol. 101, 220–230.
#'
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology.
#' For. Ecol. Manage. 331, 135-143.
#'
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion.
#' Ecology, 75(6), 1794-1806.
#'
#' @export
read_parameters <- function(file, return_list = FALSE, ...) {

  parameters <- tibble::as_tibble(utils::read.table(file, header = TRUE, ...))

  if (return_list) {

    parameters <- as.list(parameters)
  }

  # return result
  return(parameters)
}
