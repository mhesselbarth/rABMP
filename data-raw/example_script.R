library(rABMP)

names(rABMP::example_input_data)

data_trees <- prepare_input(input = example_input_data,
                            x = "x_coord", y = "y_coord",
                            species = "spec", type = "Class",
                            dbh = "bhd")

data_trees <- data_trees[1:10, ]

years <- 5

for(i in 1:years){
  data_trees <- update_competition_index(input = data_trees, standardized = TRUE)
  data_trees <- simulate_growth(input = data_trees)
  # trees <- simulate_seed_dispersal(input = data_trees) # Bug somewhere
  # trees <- simulate_mortality(data_trees) # Bug somewhere
  print(paste0(i, " from ", years, " runs done"))
}

data_trees$data[[1]]
# A tibble: 11 x 4
# i Type    DBH     CI
# <dbl> <chr> <dbl>  <dbl>
#  1    0. Adult  49.3 0.119        # Why is there such a big difference betwwen i=1 and i=2 ?
#  2    1. Adult  49.4 0.0800
#  3    2. Adult  49.5 0.0802
#  4    3. Adult  49.7 0.0805
#  5    4. Adult  49.8 0.0808
#  6    5. Adult  49.9 0.0811
#  7    6. Adult  50.1 0.0814
#  8    7. Adult  50.2 0.0817
#  9    8. Adult  50.3 0.0820
# 10    9. Adult  50.5 0.0823
# 11   10. Adult  50.6 0.0823
