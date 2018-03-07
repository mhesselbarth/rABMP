#' Update competition index
#'
#' The function updates the competition index of the provided pattern using an Epanechnikov kernel. The competition
#' kernel idea is mainly based on Pommering et al. (2011) and Pommering et al. (2014).
#' @param input [\code{tibble(1)}]\cr Tibble with individuals
#' @param standardized [\code{logical(1)}]\cr Standardize maximum CI to 1
#' @param max_dist [\code{numeric(1)}]\cr Maximum interaction distance between trees
#'
#' @export
Simulate.Competition.Index <- function(input, standardized=T, max_dist=30){

  dead <- dplyr::filter(input, Type=="Dead")
  living <- dplyr::filter(input, Type=="Alive")

  foo <- function(x, i, dbh, max_dist){
    sum(ifelse(x<max_dist,
                 yes=ifelse(x==0,
                            yes=0,
                            no=rABMP::Competition.Kernel(r=x,
                                                         dbh=dbh[i],
                                                         max_dist=max_dist)),
                 no=0))
  }

  competition <- living %>%
    dplyr::select(x,y) %>%
    dist() %>%
    as.matrix() %>%
    tibble::as.tibble() %>%
    purrr::map_dbl(foo, i=seq_along(.), dbh=living$DBH, max_dist=max_dist) %>%
    as.double()

  if(standardized==T){
    competition <- competition/max(competition)
  }

  living$CI <- competition # Update pattern
  dead$CI <- 0

  result <- dplyr::bind_rows(living, dead)

  return(result)
}

