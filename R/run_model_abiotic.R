#' run_model_abiotic
#'
#' @description Run the model
#'
#' @param data Data.table with input data.
#' @param parameters List with all parameters..
#' @param abiotic RasterLayer with abiotic conditions.
#' @param plot_area The plot area as \code{\link{owin}} object from the \code{spatstat} package.
#' @param years Numeric timesteps (years) the model runs.
#' @param save_each Integer value specifying time step results are saved.
#' @param return_seedlings Logical if seeds should be included in final output.
#' @param return_nested Logical if TRUE the final tibble is nested.
#' @param return_tibble Logical if tibble should be returned
#' @param verbose If TRUE, prints progress report.
#'
#' @details
#' Wrapper function to run the model. Executes (i) simulate_ci (ii) simulate_growth
#' (iii) simulate_seed_dispersal (iv) simulate_mortality. The input data must be
#' preprocessed using \code{prepare_input}.
#'
#' Parameters include ....
#'
#' @return data.table or tibble
#'
#' @examples
#' \dontrun{
#' df_trees <- prepare_data(data = example_input_data,
#' x = "x_coord", y = "y_coord", species = "spec", type = "Class", dbh = "bhd")
#'
#' threshold <- quantile(df_trees$dbh, probs = 0.8)
#'
#' plot_area <- spatstat::owin(xrange = c(0, 500), yrange = c(0, 500))
#'
#' ppp_threshold <- spatstat::ppp(x = df_trees[dbh > threshold, x],
#' y = df_trees[dbh > threshold, y],
#' window = plot_area)
#'
#' hetero <- spatstat::density.ppp(ppp_threshold,  dimyx = c(250, 250))
#' hetero_df <- tibble::as_tibble(hetero)
#' hetero_ras <- raster::rasterFromXYZ(hetero_df)
#'
#' parameters <- read_parameters(file = "inst/parameters.txt", sep = ";")
#'
#' parameters$growth_abiotic <- 1
#'
#' result <- run_model_abiotic(data = df_trees, parameters = parameters, years = 10,
#' abiotic = hetero_ras, plot_area = plot_area)
#' }
#'
#' @aliases run_model_abiotic
#' @rdname run_model_abiotic
#'
#' @export
run_model_abiotic <- function(data, parameters, abiotic,
                              plot_area = NULL,
                              years, save_each = NULL,
                              return_seedlings = FALSE,
                              return_nested = FALSE, return_tibble = TRUE,
                              verbose = TRUE) {

  # create one deep copy
  data <- data.table::copy(data)

  # check if input data cols are correct
  if (!all(names(data) == c("id", "i", "x", "y", "type", "dbh", "ci"))) {

    stop("Please check your input data again. See ?prepare_data for help.",
         call. = FALSE)
  }

  # check if types are correct
  if (!all(unique(data$type) %in% c("adult", "dead", "sapling", "seedling"))) {

    stop("The type of the individuals must be one of: 'adult', 'dead', 'sapling', 'seedling',",
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

  # create owin if not provided as box including all points
  if (is.null(plot_area)) {

    plot_area <- spatstat::owin(xrange = range(data$x),
                                yrange = range(data$y))

    plot_area_verbose <- TRUE
  }

  else {

    plot_area_verbose <- FALSE
  }

  # print model run information
  if (verbose) {

    message("> Using '", deparse(substitute(parameters)), "' as parameter file.")

    if (plot_area_verbose) {

      message("> Creating plot area using range of x and y coordinates.")
    }

    else {

      message("> Using '", deparse(substitute(plot_area)), "' as plot area.")
    }

    message("> Simulating ", years, " year(s). Saving results each ", save_each, " year(s).")

    if (!return_seedlings) {

      message("> Removing all seedlings for output.")
    }

    else {

      message("> Including all seedlings for output.")
    }

    if (return_nested) {

      message("> Returning nested tibble.")
    }

    else {

      message("> Returning unnested tibble.")
    }

    message("")

    message("> ...Starting simulation...")
  }

  # extract abiotic values
  abiotic_values <- rabmp::extract_abiotic(data,
                                           abiotic = abiotic)

  # initialse abiotic col
  data[, abiotic := abiotic_values]

  # loop through all years
  for (i in 1:years) {

    data <- rabmp::update_i(data)

    data <- rabmp::simulate_ci(data, parameters = parameters)

    data <- rabmp::simulate_growth_abiotic(data, parameters = parameters)

    data <- rabmp::simulate_seed_dispersal_abiotic(data, parameters = parameters,
                                                   plot_area = plot_area,
                                                   abiotic = abiotic)

    data <- rabmp::simulate_mortality(data, parameters = parameters)

    if (i %% save_each == 0) {

      data <- rabmp::update_save_each(data, save_each = save_each)
    }

    # print progress message
    if (verbose) {
      message("\r> Progress: ", i, "/", years, " simulation runs \t\t\t",
              appendLF = FALSE)
    }
  }

  # new line after last progress message
  if (verbose) {
    message("")
  }

  # remove seeds from output
  if (!return_seedlings) {

    data <- data[dbh > 1]
  }

  # order by id and i
  data.table::setorder(data, id, i)

  # conver to tibble
  if (return_tibble) {

    data <- tibble::as_tibble(data)

    # nest tibble
    if (return_nested) {

      data <- tidyr::nest(data, -c(id, x, y), .key = "data")
    }
  }

  else {

    if (return_nested) {

      message("> return_nested = TRUE only possible if return_tibble = TRUE.")
    }
  }

  return(data)
}
