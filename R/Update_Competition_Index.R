#' update_competition_index
#'
#' @details
#' Update the competition index
#'
#' @param input Tibble with input data
#' @param type Kernel type to use (either "fractional", "exponential" or "epanechnikov")
#' @param max_dist Maximum interaction distance between trees
#' @param standardized Standardize maximum CI to 1
#'
#' @references \itemize{
#' \item Pommerening, A., LeMay, V., Stoyan, D., 2011. Model-based analysis of the influence of ecological processes on forest point pattern formation-A case study. Ecol. Modell. 222, 666–678.
#' \item Pommerening, A., Maleki, K., 2014. Differences between competition kernels and traditional size-ratio based competition indices used in forest ecology. For. Ecol. Manage. 331, 135–143.
#' }
#'
#' @export
update_competition_index <- function(input,
                                     type = "exponential",
                                     max_dist = 30,
                                     standardized){

  input_unnested <- tidyr::unnest(input) # unnest data

  past <- dplyr::filter(input_unnested, i != max(i)) # data of past time steps

  current <- dplyr::filter(input_unnested, i == max(i)) # data of current time step

 # distance_matrix <- as.matrix(dist(dplyr::select(current, x, y)))# distance between all trees

  coordinates <- dplyr::select(current, x, y)
  competition <- rep(NA, length(coordinates$x))
  for(i in 1:length(coordinates$x)){
  distance <-  pointDistance(c(coordinates$x[i], coordinates$y[i]), coordinates, lonlat = F)
  competition[i] <-  sum(calculate_competition_index(distance = distance,
                              dbh = current$dbh,
                              max_dist = max_dist,
                              type = type))
  }



  # calculate competition for every col in distance matrix (equals rows of current)

  alpha <- 1.45772
  comp_trans <- competition/(current$dbh^alpha+competition) # pommerening 2014: transformation of competition index which includes size of affected tree
                                                              # scaled between 0 and 1
  # standarize results to max(competition) = 1
  if(standardized == TRUE){
     competition <- competition / max(competition)
   }

  # update tibble
  current <- dplyr::mutate(current, ci = comp_trans)

  # combine tibbles
  full_updated <- dplyr::bind_rows(current, past)

  # nest tibble
  result <- tidyr::nest(full_updated, -c(id, x, y, species), .key="data")

  return(result)
}

