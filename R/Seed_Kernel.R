#' Seed kernel
#'
#' The kernel function
#' @param species [\code{string(1)}]\cr Species of tree
#' @param distance [\code{numeric(1)}]\cr Distance r from mother tree
#' @param max_dist [\code{numeric(1)}]\cr Maximum distance of seed dispersal
#' @references \itemize{
#' \item Ribbens, E., Silander, J.A., Pacala, S.W., 1994. Seedling recruitment in forests: Calibratiing models to predict patterns of tree seedling dispersion. Ecology 75, 1794–1806.
#' }
#'
#' @export
Seed.Kernel <- function(species, distance, max_dist){
  # Rather us integrate than loop to get normalizer ?
  n <- 0

  if(species == "Beech"){
    kernel_species <- exp(-3.412413 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
      n <- n + exp(-3.412413 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species
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
    kernel_species <- exp(-3.412413 / 10 ^ 5 * r ^ 3)
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
