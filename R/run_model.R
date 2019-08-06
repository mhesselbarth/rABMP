#' run_model
#'
#' @description Run the model
#'
#' @param data Dataframe with input data.
#' @param years Numeric timesteps (years) the model runs.
#' @param verbose If TRUE, prints progress report.
#' @param parameters List with all parameters. See details for more information.
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
#' run_model(data = data_trees, years = 5)
#' }
#'
#' @aliases run_model
#' @rdname run_model
#'
#' @export
run_model <- function(data, years, parameters = NULL, verbose = TRUE) {

  if (is.null(parameters)) {

    message("> Using default parameters.")

    parameters <- construct_parameters()
  }

  for (i in 1:years) {
    data <- rabmp::simulate_ci(data)
    data <- rabmp::simulate_growth(data)
    data <- rabmp::simulate_seed_dispersal(data)
    data <- rabmp::simulate_mortality(data)

    if (verbose) {
      message("\r> Progress: ", i, "/", years, "\t\t\t", appendLF = FALSE)
    }
  }

  if (verbose) {
    message("")
  }

  return(data)
}
