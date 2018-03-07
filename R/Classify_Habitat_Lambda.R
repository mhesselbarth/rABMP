#' Classifiy habitats
#'
#' Classifies habitats based on the point density
#' @param pattern [\code{ppp(1)}]\cr Point pattern of the spatstat-package
#' @param species [\code{character(1)}]\cr Name of the species
#' @param kernel [\code{string(1)}]\cr Type of the kernel to use (see spatstat::density.ppp)
#' @param sigma [\code{numeric(1)}]\cr Sigma of density kernel (see spatstat::density.ppp)
#' @param resolution [\code{numeric(1)}]\cr Patch size of resulting raster (see raster::aggregate)
#' @param resolution [\code{numeric(1)}]\cr Patch size of resulting raster (see raster::aggregate)
#' @param n [\code{numeric(1)}]\cr Number of classes (see classInt::classIntervals)
#' @param method [\code{string(1)}]\cr Method to classifiy values (see classInt::classIntervals)
#' @return Returns a raster of the raster-package with the classified habitats
#' @examples
#' habitats_beech <- Classify.Habitats.Lambda(pattern=pattern_1999, species="Beech")
#'
#' @export
Classify.Habitats.Lambda <- function(pattern, species, kernel="epanechnikov", sigma=50, resolution=20, n=5, method="jenks"){

  pattern_species <- spatstat::subset.ppp(pattern, Species==species)
  lambda_species <- spatstat::density.ppp(pattern_species, kernel=kernel, sigma=sigma)

  df_species <- as.data.frame.im(lambda_species)
  raster_species <- rasterFromXYZ(df_species)
  resolution_raster_species <- res(raster_species)
  raster_species <- aggregate(raster_species, c(resolution/resolution_raster_species[1],
                                                resolution/resolution_raster_species[2]))

  values_raster_species <- na.omit(raster::getValues(raster_species))
  breaks_raster_species <- classInt::classIntervals(values_raster_species, n=n, style=method)

  habitats_species <-  raster::as.factor(raster::cut(raster_species, breaks=breaks_raster_species$brks,
                                                     include.lowest=T))

  return(habitats_species)
}
