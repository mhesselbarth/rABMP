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
#' @examples
#' \dontrun{
#' df_trees <- prepare_data(data = example_input_data,
#' x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#'
#' threshold <- quantile(df_trees$dbh, probs = 0.8)
#'
#' plot_area <- spatstat::owin(xrange = c(0, 500), yrange = c(0, 500))
#'
#' ppp_threshold <- spatstat::ppp(x = df_trees[dbh > threshold, x],
#' y = df_trees[dbh > threshold, y],
#' window = plot_area)
#'
#' hetero <- spatstat::density.ppp(ppp_threshold,  dimyx = c(250, 250))
#' hetero_df <- tibble::as_tibble(hetero)
#' hetero_ras <- raster::rasterFromXYZ(hetero_df)
#'
#' extract_abiotic(data = df_trees, abiotic = hetero_ras)
#'
#' }
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
