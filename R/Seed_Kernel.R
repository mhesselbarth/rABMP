#' Seed kernel
#'
#' The kernel function
#' @param species Species of tree
#' @param distance Distance r from mother tree
#' @param max_dist Maximum distance of seed dispersal
#' @references \itemize{
#' \item Ribbens, E., Silander, J.A., Pacala, S.W., 1994. Seedling recruitment in forests: Calibrating models to predict patterns of tree seedling dispersion. Ecology 75, 1794–1806.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#'
#' @export
seed_kernel <- function(species, distance, max_dist){
  # Rather us integrate than loop to get normalizer ?
  n <- 0

  if(species == "Beech"){
    kernel_species <- exp(-3.412413 / 10 ^ 5 * distance ^ 3)
    for(i in 0:max_dist){
    n <- n + exp(-3.412413 / 10 ^ 5 * i ^ 3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1 / n) * kernel_species

    # Probability based on Milleron et al. (2013) Year 2007
    #beta <- runif(n = 1, min = 197.9, max =  244.7)
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
