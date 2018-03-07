#' Random Coordinates
#'
#' Create Random coordinates depending on seed kernel function
#'
#' @export
Random.Coordinates <- function(species, n=100, r_max=80, number_samples=1000000){

  result <- tibble::tibble(Proposed=runif(number_samples, min=0, max=r_max)) %>%
    dplyr::mutate(Target=Seed.Kernel(species=species, r=Proposed, r_max=r_max),
                  Random=runif(number_samples, min=0, max=1),
                  Accept=dplyr::case_when(Random <= Target/max(Target, na.rm=T) ~ TRUE,
                                          TRUE ~ FALSE)) %>%
     dplyr::filter(Accept==TRUE) %>%
     dplyr::pull(Proposed) %>%
     c(. , . * -1)

  return(result)
}

