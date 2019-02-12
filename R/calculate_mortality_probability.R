#' calculate_mortality_probability
#'
#' @description Calculate mortality probability
#'
#' @param species current species
#' @param dbh of current tree
#'
#' @details
#' Calculate the mortality probability depending on the species and the current DBH.
#'
#' @return vector
#'
#' @examples
#' calculate_mortality_probability(species = "Beech", dbh = 25.31)
#'
#' @aliases calculate_mortality_probability
#' @rdname calculate_mortality_probability
#'
#' @references
#' Holzwarth, F., Kahl, A., Bauhus, J., & Wirth, C. (2013). Many ways to die - partitioning
#' tree mortality dynamics in a near-natural mixed deciduous forest. Journal of Ecology, 101(1), 220-230.
#'
#' DO NOT EXPORT
# calculate_mortality_probability <- function(species, dbh){
#
#   if(species == "Beech"){
#
#     # calculate dbh increase
#     dbh_inc <- exp(-3.4 + 2.1 * (1 - exp(-(-0.00035) * dbh ^ 2.5 ) ) )
#
#     # calculate logit for "early phase"
#     logit_early <- ifelse(test = is.na(1.8 + (-2.1)*log(dbh + 8) + (dbh_inc * -1.4)),
#                           yes = 0, no = 1.8 + (-2.1)*log(dbh + 8) + (dbh_inc * -1.4))
#
#     # calculate logit for late phase
#     logit_late <- -8.9 + (dbh * 0.052)
#
#     # sum both logits
#     p_early <- 1 / (1 + exp(-logit_early))
#     p_late <- 1 / (1 + exp(-logit_late))
#
#     # calculate prob
#     p <- p_early + p_late
#   }
#
#   else if(species == "Ash"){
#
#     # calculate logit
#     logit <- 1.3 + (log(dbh) * -1.6)
#
#     # calculate prob
#     p <- 1 / (1 + exp(-logit))
#   }
#
#   else if(species == "Hornbeam"){
#
#     # calculate logit
#     logit <- -2.8 + (dbh * -0.051)
#
#     # calculate prob
#     p <- 1 / (1 + exp(-logit))
#   }
#
#   else if(species == "Sycamore"){
#
#     # calculate logit
#     logit <- -8.9 + (dbh * 0.052)
#
#     # calculate prob
#     p <- 1 / (1 + exp(-logit))
#   }
#
#   else if(species == "others"){
#
#     # calculate logit
#     logit <- -8.9 + (dbh * 0.052)
#
#     # calculate prob
#     p <- 1 / (1 + exp(-logit))
#   }
#
#   else{
#     print("Please select valid species - returning 0")
#     p <- 0
#   }
#
#   return(p)
# }
