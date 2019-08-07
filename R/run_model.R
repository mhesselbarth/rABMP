#' run_model
#'
#' @description Run the model
#'
#' @param data Dataframe with input data.
#' @param years Numeric timesteps (years) the model runs.
#' @param ... Possibility to change the value of parameters.
#' @param verbose If TRUE, prints progress report.
#'
#' @details
#' Wrapper function to run the model. Executes (i) simulate_ci (ii) simulate_growth
#' (iii) simulate_seed_dispersal (iv) simulate_mortality. The input data must be
#' preprocessed using \code{prepare_input}.
#'
#' Parameters include ....
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' data_trees <- prepare_data(data = example_input_data,
#' x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#'
#' result <- run_model(data = data_trees, years = 10)
#' }
#'
#' @aliases run_model
#' @rdname run_model
#'
#' @export
run_model <- function(data, years, ..., verbose = TRUE) {

  parameters <- rabmp::construct_parameters(..., verbose = verbose)

  for (i in 1:years) {
    data <- rabmp::simulate_ci(data, parameters = parameters)
    data <- rabmp::simulate_growth(data, parameters = parameters)
    data <- rabmp::simulate_seed_dispersal(data, parameters = parameters)
    data <- rabmp::simulate_mortality(data, parameters = parameters)

    if (verbose) {
      message("\r> Progress: ", i, "/", years, "\t\t\t", appendLF = FALSE)
    }
  }

  if (verbose) {
    message("")
  }

  return(data)
}
