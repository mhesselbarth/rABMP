#' calculate_ci
#'
#' @description Calculate competition index
#'
#' @param distance distance between focal tree and all others
#' @param dbh DBH of trees
#' @param max_dist maximum interaction distance
#' @param type kernel type to use (either "fractional", "exponential" or "epanechnikov")
#'
#' @details
#' Calculate competition index based on distances to neighbouring trees and dbh of
#' neighbours. Neigbours with a distance larger than \code{max_dist} are not considered.
#' It is used to standartize values to 0-1 for Epanechnikov kernel
#'
#' @return vector
#'
#' @aliases calculate_ci
#' @rdname calculate_ci
#'
#' @references
#' Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence
#' of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666-678.
#'
#' Pommerening, A., Maleki, K., 2014. Differences between competition kernels and
#' traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135-143.
#'
#' DO NOT EXPORT
# calculate_ci <- function(distance, dbh, max_dist, type = "exponential"){
#
#   if(type == "fractional"){
#
#     # set parameters
#     alpha <- 3.24074
#     beta <- 1.05879
#
#     # calculate competition
#     competition <- (dbh ^ alpha) / (1 + ((distance / beta) ^ 2))
#
#     # set all above distance threshold and competition on itself to 0
#     # competition[which(distance > max_dist | distance == 0)] <- 0
#   }
#
#   else if(type == "exponential"){
#
#     # set parameters
#     alpha <- 1.45772
#     beta <- 0.52339
#
#     # calculate competition
#     competition <- (dbh ^ alpha) * exp( - (distance / (dbh ^ beta)))
#
#     # set all above distance threshold and competition on itself to 0
#     # competition[which(distance > max_dist | distance == 0)] <- 0
#   }
#
#   else if(type == "epanechnikov"){
#
#     # standarize to max_dist = 1
#     distance_standardized <- distance / max_dist
#
#     # calculate kernel value
#     kernel <- (3 / 4) * (1 - (distance_standardized ^ 2))
#
#     # weighting by dbh
#     # MH: Do we still need this?
#     competition <- kernel * dbh
#
#     # set all above distance threshold and competition on itself to 0
#     # competition[which(distance > max_dist | distance == 0)] <- 0
#   }
#
#   else{
#     print("Please select valid kernel ('Fractional', 'Exponential' or 'Epanechnikov') - returning CI=0")
#     competition <- 0
#   }
#
#   return(competition)
# }
