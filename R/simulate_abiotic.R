#' extract_abiotic
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
#' @aliases extract_abiotic
#' @rdname extract_abiotic
#'
#' @references
#' ADD TEXT HERE
#'
#' @export
extract_abiotic <- function(data, abiotic){

  # get id of current living and no seedlings
  # id <- data[type != "dead" & i == max(i), which = TRUE]

  # extract abiotic value
  abiotic_value <- raster::extract(x = abiotic,
                                   y = as.matrix(data[, c("x", "y")]))

  # somehow scale abiotic value
  # abiotic_value <- abiotic_value / max(abiotic_value)

  return(abiotic_value)
}
