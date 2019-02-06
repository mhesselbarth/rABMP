###########################
# Create example data set #
###########################

library(rABMP)
library(dplyr)
library(raster)
set.seed(42)

 # species <- c("Beech", "Ash", "Hornbeam", "Sycamore", "others")
 # type <- c("Adult", "Dead")
 # example_input_data <- spatstat::rThomas(kappa=0.001, scale=10, mu=5,
 #                   win=spatstat::owin(c(0,500), c(0,500))) %>%
 #   as.data.frame() %>%
 #   setNames(c("x_coord", "y_coord")) %>%
 #   tibble::as.tibble() %>%
 #   dplyr::mutate(spec = sample(x=species, size=n(),
 #                                  prob=c(0.90092535, 0.03896194, 0.02713421,
 #                                         0.02226397, 0.01071453), replace=T),
 #                 bhd = rnorm(n=n(), mean=21.12675, sd=21.39956),
 #                 bhd = replace(bhd, bhd<1, 1),
 #                 Class = sample(x=type, size=n(),
 #                                prob=c(0.8783135, 0.1216865), replace=T),
 #                 Class = case_when(Class != "Dead" & bhd <= 10 ~ "Seedling",
 #                                   Class != "Dead" & bhd > 10 ~ "Adult",
 #                                   Class == "Dead" ~ "Dead"))
 #
 #
 # usethis::use_data(example_input_data, compress="xz", overwrite=TRUE)
input_data <- readRDS("C:/Users/Lena/Desktop/projektarbeit/Daten/pattern_1999_reconstructed_20000.rds")
input_data <- as.data.frame(input_data)
input_data$Species <- as.character(input_data$Species)
input_data$Type <- as.character(input_data$Type)
input_data <- dplyr::mutate(input_data,
                            Type = case_when(Type != "dead" & DBH <= 10 ~ "Seedling",
                                             Type != "dead" & DBH > 10 ~ "Adult",
                                             Type == "dead" ~ "Dead"))
input_data <- input_data[sample(1:nrow(input_data)), ]

usethis::use_data(input_data, compress="xz", overwrite=TRUE)

#hist(input_data$DBH, xlab="dbh", ylab="frequency", main="dbh distribution", col="gray")

#plot(example_input_data$x_coord, example_input_data$y_coord)
