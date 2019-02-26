
# # check names of input data
# names(rabmp::input_data)
#
# # prepare input (mainly renameing and nesting)
# data_trees <- prepare_input(input = input_data,
#                             x = "x", y = "y",
#                             species = "Species", type = "Type",
#                             dbh = "DBH")

# check names of input data
names(rabmp::example_input_data)

# prepare input (mainly renameing and nesting)
data_trees <- prepare_input(input = example_input_data,
                            x = "x_coord", y = "y_coord",
                            species = "spec", type = "Class",
                            dbh = "bhd")

# only 200 trees to decrease computationl time for testing
data_trees <- data_trees[0:50, ]

# set number of simulation years
years <- 5

for(i in 1:years){
  data_trees <- simulate_ci(data_trees)
  data_trees <- simulate_growth(data_trees)
  data_trees <- simulate_seed_dispersal(data_trees)
  data_trees <- simulate_mortality(data_trees)
  print(paste0(i, " from ", years, " runs done"))
}

data_trees$data[1:20]

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

bench::mark(
  simulate_seed_dispersal(data_trees),
  simulate_seed_dispersal(data_trees),
  check = FALSE, relative = TRUE, iterations = 10)
