#' deprecated_calculate_seed_kernel
#'
#' @description Calculate seed kernel
#'
#' @param species Current species
#' @param distance distance for which probability is calculated
#' @param max_dist maximum distance
#'
#' @details
#' Calculates the probability for a certain distance a species-specific
#' seed kernel.
#'
#' @return vector
#'
#' @examples
#' deprecated_calculate_seed_kernel(species = "Beech", distance = 10, max_dist = 120)
#'
#' @aliases deprecated_calculate_seed_kernel
#' @rdname deprecated_calculate_seed_kernel
#'
#' @references
#' Ribbens, E., Silander, J. A., & Pacala, S. W. (1994). Seedling recruitment in forests:
#' Calibrating models to predict patterns of tree seedling dispersion. Ecology, 75(6), 1794-1806.
#'
#' @export
deprecated_calculate_seed_kernel <- function(species, distance, max_dist) {

  # Rather us integrate than loop to get normalizer ?
  n <- 0

  if(species == "Beech"){
    kernel_species <- exp(-3.412413 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
    n <- n + exp(-3.412413 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species

    # Probability based on Milleron et al. (2013) Year 2007
    #beta <- stats::runif(n = 1, min = 197.9, max =  244.7)
    #kernel_return <- 1 / (pi * beta * (1 + (distance^2 / beta))^2)

  }

  else if(species == "Ash"){
    kernel_species <- exp(-0.922805 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
      n <- n + exp(-0.922805 / 10 ^ 5* i ^ 3) # n <- integrate(function(r){exp(-0.922805/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species
  }

  else if(species == "Sycamore"){
    kernel_species <- exp(-7.435026 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
      n <- n + exp(-7.435026 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-7.435026/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species
  }

  else if(species == "Hornbeam"){ # same as Beech
    kernel_species <- exp(-3.412413 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
      n <- n + exp(-3.412413 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species
  }

  else if(species=="others"){ # mean all other species
    kernel_species <- exp(-3.795664 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
      n <- n + exp(-3.795664 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-3.795664/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species
  }

  else{
    print("Please select valid species - kernel=0 returned")
    kernel_species <- 0
  }

  return(kernel_return)
}
