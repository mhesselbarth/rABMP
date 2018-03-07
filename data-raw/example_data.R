###########################
# Create example data set #
###########################

set.seed(42)

species <- c("Beech", "Ash", "Hornbeam", "Sycamore", "others")
type <- c("Alive", "Dead")

example_input_data <- spatstat::rThomas(kappa=0.001, scale=10, mu=5,
                  win=spatstat::owin(c(0,500), c(0,500))) %>%
  as.data.frame() %>%
  tibble::as.tibble() %>%
  dplyr::mutate(Species = sample(x=species, size=n(),
                                 prob=c(0.75, 0.1, 0.05, 0.05, 0.05), replace=T),
                Type = sample(x=type, size=n(),
                              prob=c(0.9, 0.1), replace=T),
                DBH = rnorm(n=n(), mean=45, sd=25),
                DBH = replace(DBH, DBH<1, 1))

devtools::use_data(example_input_data, compress="xz", overwrite=TRUE)


