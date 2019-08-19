#' run_model
#'
#' @description Run the model
#'
#' @param data Dataframe with input data.
#' @param parameters List with all parameters.
#' @param years Numeric timesteps (years) the model runs.
#' @param save_each Integer value specifying time step results are saved.
#' @param seed_dispersal Logical if seed dispersal should be simulated.
#' @param return_nested Logical if TRUE the final tibble is nested.
#' @param plot_area The plot area as \code{\link{owin}} object from the \code{spatstat} package.
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
#' df_trees <- prepare_data(data = example_input_data,
#' x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#'
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = "\t", return_list = TRUE)
#'
#' result <- run_model(data = df_trees, parameters = parameters, years = 10)
#' }
#'
#' @aliases run_model
#' @rdname run_model
#'
#' @export
run_model <- function(data, parameters, years, save_each = NULL,
                      seed_dispersal = TRUE, plot_area = NULL,
                      return_nested = TRUE, verbose = TRUE) {

  # check if input data cols are correct
  if (!all(names(data) == c("id", "i", "x", "y", "species", "type", "dbh", "ci"))) {

    stop("Please check your input data again. See ?prepare_data for help.",
         call. = FALSE)
  }

  # check if types are correct
  if (!all(unique(data$type) %in% c("adult", "dead", "sapling", "seedling"))) {

    stop("The type of the individuals must be one of: 'adult', 'dead', 'sapling', 'seedling',",
         call. = FALSE)
  }

  # check if species are correct
  if (!all(unique(data$species) %in% c("beech", "ash", "hornbeam", "sycamore", "others"))) {

    stop("The species of the individuals must be one of: 'beech', 'ash', 'hornbeam', 'sycamore' or 'others'.",
         call. = FALSE)
  }

  # check if save_each is present
  if (is.null(save_each)) {

    save_each <- 1
  }

  # check if years can be divided by provided save_each without remainder
  else{

    if (years %% save_each != 0) {

      warning("'years' cannot be divided by 'save_each' without remainder.",
              call. = FALSE)
    }
  }

  # print save_each
  if (verbose) {

    message("> Saving results of save_each = ", save_each, ".")
  }

  # create owin if not provided as box including all points
  if (is.null(plot_area)) {

    if (verbose) {

      message("> Creating plot_area using spatstat::owin(xrange = range(data$x), yrange = range(data$y))")
    }

    plot_area <- spatstat::owin(xrange = range(data$x),
                                yrange = range(data$y))
  }

  else {

    if (verbose) {

      message("> Using '", deparse(substitute(plot_area)), "' as plot area.")
    }

  }

  # loop through all years
  for (i in 1:years) {

    data <- rabmp::update_i(data)

    data <- rabmp::simulate_ci(data, parameters = parameters)

    data <- rabmp::simulate_growth(data, parameters = parameters)

    if (seed_dispersal) {

      data <- rabmp::simulate_seed_dispersal(data, parameters = parameters, plot_area = plot_area)
    }

    data <- rabmp::simulate_mortality(data, parameters = parameters)

    if (i %% save_each == 0) {

      data <- rabmp::update_save_each(data, save_each = save_each)
    }

    # print progress message
    if (verbose) {
      message("\r> Progress: ", i, "/", years, "\t\t\t", appendLF = FALSE)
    }
  }

  # new line after last progress message
  if (verbose) {
    message("")
  }

  # order by id and i
  data <- data.table::setorder(data, id, i)

  # conver to tibble
  data <- tibble::as_tibble(data)

  # nest tibble
  if (return_nested) {

    data <- tidyr::nest(data, -c(id, x, y, species), .key = "data")
  }

  return(data)
}
