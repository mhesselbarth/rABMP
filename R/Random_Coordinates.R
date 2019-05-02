#' Random Coordinates
#'
#' Create Random coordinates depending on seed kernel function
#'
#' @export
Random.Coordinates <- function(species, n=100, r.max=80, number.samples=1000000){
  df_samples <- data.frame(Proposed=runif(number.samples, min=0, max=r.max))
  df_samples$Target <- Seed.Kernel(species=species, r=df_samples$Proposed, r.max=r.max)
  df_samples$Random <- runif(number.samples, min=0, max=1)
  df_samples$Accepted <- with(df_samples, Random <= Target/max(df_samples$Target, na.rm=T))
  df_samples <- df_samples[df_samples$Accepted==T,]

  sample_coordinates <- data.frame(Coordinates=sample(x=df_samples$Proposed, size=n, replace=T), Random=runif(n))
  sample_coordinates$Coordinates[sample_coordinates$Random<=0.5] <- sample_coordinates$Coordinates[sample_coordinates$Random<=0.5] * -1

  return(sample_coordinates$Coordinates)
}

