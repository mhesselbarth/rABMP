#' #' deprecated_calculate_ci
#' #'
#' #' @description Calculate competition index
#' #'
#' #' @param data matrix with x-coordinate, y-coordinate and dbh
#' #' @param alpha parameter competition kernel
#' #' @param beta parameter competition kernel
#' #' @param max_dist maximum interaction distance
#' #'
#' #' @details
#' #' Calculate competition index based on distances to neighbouring trees and dbh of
#' #' neighbours. Neigbours with a distance larger than \code{max_dist} are not considered.
#' #' It is used to standartize values to 0-1 for Epanechnikov kernel
#' #'
#' #' @return vector
#' #'
#' #' @aliases deprecated_calculate_ci
#' #' @rdname deprecated_calculate_ci
#' #'
#' #' @references
#' #' Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence
#' #' of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666-678.
#' #'
#' #' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' #' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#' #'
#' #' @export
#' deprecated_calculate_ci <- function(data,
#'                                     alpha,
#'                                     beta,
#'                                     max_dist){
#'
#'   ci_r <- rep(NA, nrow(data))
#'
#'   for(i in 1:nrow(data)) {
#'
#'     # calculate distance between current point and all other points
#'     distance <- deprecated_calculate_distance(point_a = data[i, 1:2, drop = FALSE],
#'                                               point_b = data[, 1:2])
#'
#'     dbh <- data[which(distance < max_dist & distance != 0), 3]
#'
#'     distance <- distance[which(distance < max_dist & distance != 0)]
#'
#'     # calculate competition of current tree
#'     competition <- sum((dbh ^ alpha) * exp( - (distance / (dbh ^ beta))))
#'     # competition[which(distance > max_dist | distance == 0)] <- 0
#'
#'     ci_r[i] <- competition
#'   }
#'
#'   return(ci_r)
#' }
