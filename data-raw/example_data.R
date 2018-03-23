###########################
# Create example data set #
###########################

set.seed(42)

species <- c("Beech", "Ash", "Hornbeam", "Sycamore", "others")
type <- c("Adult", "Dead")

example_input_data <- spatstat::rThomas(kappa=0.001, scale=10, mu=5,
                  win=spatstat::owin(c(0,500), c(0,500))) %>%
  as.data.frame() %>%
  setNames(c("x_coord", "y_coord")) %>%
  tibble::as.tibble() %>%
  dplyr::mutate(spec = sample(x=species, size=n(),
                                 prob=c(0.75, 0.1, 0.05, 0.05, 0.05), replace=T),
                bhd = rnorm(n=n(), mean=45, sd=25),
                bhd = replace(bhd, bhd<1, 1),
                Class = sample(x=type, size=n(),
                               prob=c(0.9, 0.1), replace=T),
                Class = case_when(Class != "Dead" & bhd <= 10 ~ "Seedling",
                                  Class != "Dead" & bhd > 10 ~ "Adult",
                                  Class == "Dead" ~ "Dead"))

devtools::use_data(example_input_data, compress="xz", overwrite=TRUE)


