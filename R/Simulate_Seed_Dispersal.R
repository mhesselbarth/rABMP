#' Seed dispersal
#'
#' The function simulates growth of individual points
#' @param pattern [\code{ppp(1)}]\cr Point pattern object of the spatstat package
#' @param dbh [\code{String(1)}]\cr Name of the DBH mark
#' @references \itemize{
#' \item Clark, J.S., Silman, M., Kern, R., Macklin, E., HilleRisLambers, J., 1999. Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80, 1475–1494.
#' \item Millerón, M., De Heredia, U.L., Lorenzo, Z., Alonso, J., Dounavi, A., Gil, L., Nanos, N., 2013. Assessment of spatial discordance of primary and effective seed dispersal of European beech (Fagus sylvatica L.) by ecological and genetic methods. Mol. Ecol. 22, 1531–1545.
#' }
#' @examples
#' CI <- Seed.dispersal(pattern=pattern_living_trees)
#'
#' @export
Simulate.Seed.Dispersal <- function(pattern, threshold=30, dbh="DBH"){

  names_pattern_regenerated <- names(pattern$marks)
  pattern_regenerated <- sample_pattern[sample_pattern$marks[dbh]>=threshold & sample_pattern$marks$Type!="dead"]
  df_seedlings_all <- cbind(pattern_regenerated$marks[0,], data.frame(x=numeric(), y=numeric()))

  for(i in 1:pattern_regenerated$n){
    dbh_i <- as.numeric(pattern_regenerated[i]$marks[dbh])
    species_i <- as.character(pattern_regenerated[i]$marks$Species)
    x_coord_i <- pattern_regenerated[i]$x
    y_coord_i <- pattern_regenerated[i]$y

    no_seedlings <- floor(Number.Seeds(species=species_i, dbh=dbh_i))

    df_seedlings_i <- data.frame(matrix(0, nrow = no_seedlings, ncol = length(names_pattern_regenerated)))
    names(df_seedlings_i) <- names_pattern_regenerated

    df_seedlings_i$DBH <- 1.0
    df_seedlings_i$Type <- "seedling"
    df_seedlings_i$Species <- species_i

    df_seedlings_i$x <- x_coord_i + Random.Coordinates(species=species_i, n=no_seedlings)
    df_seedlings_i$y <- y_coord_i + Random.Coordinates(species=species_i, n=no_seedlings)

    df_seedlings_all <- rbind(df_seedlings_all, df_seedlings_i)
  }

  df_seedlings_all$Species <- as.factor(df_seedlings_all$Species)
  df_seedlings_all$Type <- as.factor(df_seedlings_all$Type)

  pattern_seedlings <- spatstat::ppp(x=df_seedlings_all$x, y=df_seedlings_all$y,
                           window=spatstat::ripras(x=df_seedlings_all$x, y=df_seedlings_all$y, shape="rectangle"))
  marks(pattern_seedlings) <- subset(df_seedlings_all, select=-c(x,y))

  pattern_all <- spatstat::superimpose(pattern, pattern_seedlings, W=pattern$window)

  return(pattern_all)
}




