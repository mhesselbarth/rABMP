#' run_model
#'
#' @description Run the modeld
#'
#' @param data Input data frame.
#' @param years Timesteps (years) the model runs.
#'
#' @details
#' Wrapper function to run the model. Executes (i) simulate_ci (ii) simulate_growth
#' (iii) simulate_seed_dispersal (iv) simulate_mortality
#'
#' @return tibble
#'
#' @examples
#' data_trees <- prepare_input(input = example_input_data, x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#'
#' run_model(data = data_trees, years = 3)
#'
#' @aliases run_model
#' @rdname run_model
#'
#' @export
run_model <- function(data, years, verbose = TRUE) {

  for (i in 1:years) {
    data <- simulate_ci(data)
    data <- simulate_growth(data)
    data <- simulate_seed_dispersal(data)
    data <- simulate_mortality(data)

    if (verbose) {
      message("\r> Progress: ", i, "/", years, "\t\t\t", appendLF = FALSE)
    }
  }

  if (verbose) {
    message("")
  }

  return(data_trees)
}
