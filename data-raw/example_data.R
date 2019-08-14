# Create example data set #

# load libraries
library(dplyr)
library(rabmp)
library(raster)
library(spatstat)
library(tibble)

# set seed
set.seed(42)

species <- c("beech", "ash", "hornbeam", "sycamore", "others")
type <- c("adult", "dead")

# example data with similar probs than field data of old-growth forest for species, dbh and dead/living
example_input_data <- spatstat::rThomas(kappa = 0.001, scale = 10, mu = 5,
                                        win = spatstat::owin(c(0, 500), c(0, 500))) %>%
   tibble::as_tibble() %>%
   setNames(c("x_coord", "y_coord")) %>%
   dplyr::mutate(spec = sample(x = species, size = n(),
                               prob = c(0.90092535, 0.03896194, 0.02713421,
                                        0.02226397, 0.01071453), replace = TRUE),
                 bhd = sqrt(rnorm(n = n(), mean = 21.12675, sd = 21.39956) ^ 2),
                 bhd = replace(bhd, bhd < 1, 1),
                 Class = sample(x = type, size = n(),
                                prob = c(0.8783135, 0.1216865), replace = TRUE),
                 Class = case_when(Class != "dead" & bhd <= 10 ~ "sapling",
                                   Class != "dead" & bhd > 10 ~ "adult",
                                   Class == "dead" ~ "dead"))

# save data with max compression
usethis::use_data(example_input_data, compress = "xz", overwrite = TRUE)
