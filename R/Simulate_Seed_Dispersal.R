#' simulate_seed_dispersal
#'
#' @details
#' Simulate seed dispersal
#'
#' @param input Tibble with input data
#' @param threshold Minimum DBH threshold for reproduction
#'
#' @references \itemize{
#' \item Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475–1494.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#'
#' @export
simulate_seed_dispersal <- function(input, threshold = 30){

  # unnest data
  input_unnested <- tidyr::unnest(input)

  # only get living trees of current timestep above threshold
  current_living <- dplyr::filter(input_unnested,
                                  type != "Dead",
                                  i == max(i),
                                  dbh > threshold)

  # get most recent time step
  i <- max(current_living$i)

  # Number of seedlings for each tree
  no_seedlings <- purrr::map2_dbl(current_living$species, current_living$dbh,
                                  function(x, y) rabmp::number_seeds(species = x,
                                                                     dbh = y))
#creates boxplot with number of seedlings per species
  # current_living$no_seedlings <- no_seedlings
  # library(ggplot2)
  # boxplot <- ggplot(data=current_living, aes(x=species, y=no_seedlings))+ geom_boxplot()
  # boxplot + labs(y = "number of seeds")

#creates curves with number of seeds per species
# plot(no_seedlings[species=="Beech"] ~ dbh[species=="Beech"], data = current_living, xlab="dbh", ylab="number of seeds")
# points(no_seedlings[species=="Ash"] ~ dbh[species=="Ash"], data = current_living, col="darkred")
# points(no_seedlings[species=="Sycamore"] ~ dbh[species=="Sycamore"], data = current_living, col="darkgreen")
# legend("topright", legend=c("Beech", "Ash", "Hornbeam"), col=c("black", "darkred", "darkgreen"), pch=1)

  # Reduce seedlings because of browsing and general mortality

  no_seedlings <- floor(no_seedlings * runif(1, 0.812, 0.83)) #Bilek et al 2009: zwischen 17 und 18.8 % der Bucheckern waren leer
  no_seedlings <- floor(no_seedlings * 0.0236) #Bilek et al 2009: nur 2,36 % der vollen Bucheckern überlebten das erste jahr,für alle Arten gleich?


  # Create seedlings
  seedlings <- purrr::pmap_dfr(list(current_living$species, no_seedlings, current_living$x, current_living$y),
                                      function(species, n, x_coord, y_coord) {
                                        distance_x <- rabmp::random_distance(species = species, n = n)
                                        coords_x <- x_coord + distance_x

                                        distance_y <- rabmp::random_distance(species = species, n = n)
                                        coords_y <- y_coord + distance_y

                                        tibble::tibble(x = coords_x,
                                                       y = coords_y,
                                                       species = species,
                                                       i = i,
                                                       type = "Seedling",
                                                       dbh = 1.0,
                                                       ci = 0.0)
                                        })


  result <- dplyr::bind_rows(input_unnested, seedlings)

  result <- tidyr::nest(result, -c(id, x, y, species), .key="data")

  return(result)
}

# plot(current_living$x, current_living$y, pch=16, xaxt='n', yaxt='n', ann=FALSE)
# points(seedlings$x, seedlings$y, col="gray")
#
#
# plot(result$x, result$y, col="gray", xaxt='n', yaxt='n', ann=FALSE)
# points(current_living$x, current_living$y, pch=16)
