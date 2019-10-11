#' extract_abiotic
#'
#' @description Extract abiotic values
#'
#' @param data Dataframe with input data.
#' @param abiotic Raster with abiotic conditions.
#'
#' @details
#' Function to extract value of abiotic RasterLayer at location of individuals
#'
#' @return vector
#'
#' @aliases extract_abiotic
#' @rdname extract_abiotic
#'
#' @export
extract_abiotic <- function(data, abiotic){

  # extract abiotic value
  abiotic_value <- raster::extract(x = abiotic,
                                   y = as.matrix(data[, .(x, y)]))

  # somehow scale abiotic value
  # abiotic_value <- abiotic_value / max(abiotic_value)

  return(abiotic_value)
}
