#' Seed kernel
#'
#' The kernel function
#' @param species [\code{String(1)}]\cr Species of tree
#' @param r [\code{Numeric(1)}]\cr Distance r from mother tree
#' @references \itemize{
#' \item Martínez, I., González-Taboada, F., 2009. Seed dispersal patterns in a temperate forest during a mast event: Performance of alternative dispersal kernels. Oecologia 159, 389–400.#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' \item further references
#' }
#' @examples
#' CI <- Seed.dispersal(pattern=pattern_living_trees)
#'
#' @export
Seed.Kernel <- function(species, r, r.max){
  n <- 0

  if(species=="Beech"){
    kernel_species <- exp(-3.412413/10^5*r^3)
    for(i in 0:r.max){
      n <- n + exp(-3.412413/10^5*i^3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1/n) * kernel_species
  }

  else if(species=="Ash"){
    kernel_species <- exp(-0.922805/10^5*r^3)
    for(i in 0:r.max){
      n <- n + exp(-0.922805/10^5*i^3) # n <- integrate(function(r){exp(-0.922805/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1/n) * kernel_species
  }

  else if(species=="Sycamore"){
    kernel_species <- exp(-7.435026/10^5*r^3)
    for(i in 0:r.max){
      n <- n + exp(-7.435026/10^5*i^3) # n <- integrate(function(r){exp(-7.435026/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1/n) * kernel_species
  }

  else if(species=="Hornbeam"){ # same as Beech
    kernel_species <- exp(-3.412413/10^5*r^3)
    for(i in 0:r.max){
      n <- n + exp(-3.412413/10^5*i^3) # n <- integrate(function(r){exp(-3.412413/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1/n) * kernel_species
  }

  else if(species=="others"){ # mean all other species
    kernel_species <- exp(-3.795664/10^5*r^3)
    for(i in 0:r.max){
      n <- n + exp(-3.795664/10^5*i^3) # n <- integrate(function(r){exp(-3.795664/10^5*r^3)}, lower=0, upper=Inf)$value
    }
    kernel_return <- (1/n) * kernel_species
  }

  else{
    print("Please select valid species - kernel=0 returned")
    kernel_species <- 0
  }

  return(kernel_return)
}
