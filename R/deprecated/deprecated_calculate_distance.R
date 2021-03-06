#' #' deprecated_calculate_distance
#' #'
#' #' @description Calculate distance
#' #'
#' #' @param point_a coordinates of first (set of) point(s)
#' #' @param point_b coordinates of second (set of) point(s)
#' #'
#' #' @details
#' #' Calculate the distance between two (set of) point(s). Coordinates must be a
#' #' matrix with x coordinates in the first column and y coordinates in the second
#' #' column
#' #'
#' #' @return vector
#' #'
#' #' @examples
#' #' points_a <- cbind(c(5, 1, 2, 8), c(7, 9, 2, 3))
#' #' points_b <- cbind(c(3, 4, 9, 1), c(4, 8, 1, 4))
#' #' deprecated_calculate_distance(points_a, points_b)
#' #'
#' #' \dontrun{
#' #' bench::mark(deprecated_calculate_distance(points_a, points_b),
#' #' raster::pointDistance(points_a, points_b, lonlat = FALSE),
#' #' relative = TRUE, iterations = 10000)
#' #' }
#' #'
#' #' @aliases deprecated_calculate_distance
#' #' @rdname deprecated_calculate_distance
#' #'
#' #' @export
#' deprecated_calculate_distance <- function(point_a, point_b) {
#'
#'   if(class(point_a) != "matrix" || class(point_b) != "matrix") {
#'     stop("point_a and point_b must both be a matrix.")
#'   }
#'
#'   sqrt(((point_a[, 1] - point_b[, 1]) ^ 2 + (point_a[, 2] - point_b[, 2]) ^ 2))
#' }
