#' simulate_abiotic
#'
#' @description Update competition index
#'
#' @param data Dataframe with input data.
#' @param abiotic Raster with abiotic conditions.
#'
#' @details
#' ADD TEXT HERE
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' ADD TEXT HERE
#' }
#'
#' @aliases simulate_abiotic
#' @rdname simulate_abiotic
#'
#' @references
#' ADD TEXT HERE
#'
#' @export
simulate_abiotic <- function(data, abiotic){

  # get id of current living and no seedlings
  id <- data[type != "dead" & i == max(i), which = TRUE]

  # extract abiotic value
  abiotic_value <- raster::extract(x = abiotic,
                                   y = as.matrix(data[id, c("x", "y")]))

  # somehow scale abiotic value
  abiotic_value <- abiotic_value / max(abiotic_value)

  # update tibble
  data[id, abiotic := abiotic_value]

  return(data)
}

bench::mark(
as.matrix(data[id, c("x", "y")]),
cbind(data[id, x], data[id, y]),
iterations = 10000, check = FALSE)
