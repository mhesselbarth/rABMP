#' prepare_data
#'
#' @description Prepare data
#'
#' @param data Dataframe with input data.
#' @param x String with name of column containing the x-coordinate.
#' @param y String with name of column containing the y-coordinate.
#' @param type String with name of column containing the type as string.
#' @param dbh String with name of column containing the DBH as dbl.
#'
#' @details
#' The funcion modifies the structures of the data dataframe. This includes
#' renameing of the coloumns and nesting the data.
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' names(example_input_data)
#' prepare_data(data = example_input_data, x = "x_coord", y = "y_coord",
#' type = "Class", dbh = "bhd")[]
#' }
#'
#' @aliases prepare_data
#' @rdname prepare_data
#'
#' @export
prepare_data <- function(data, x, y, type, dbh){

  # convert to data.table
  data <- data.table::as.data.table(data)

  # get name of cols to select
  select_cols <- c(x, y, type, dbh)

  # select cols
  data <- data[, ..select_cols]

  # initialize competition index
  data[, ci := 0]

  # initialize time step counter
  data[, i := 0]

  # add id
  data[, id := 1:nrow(data)]

  # order columns
  data.table::setcolorder(data, c("id", "i", select_cols, "ci"))

  # name columns
  data.table::setnames(data, c("id", "i", "x", "y", "type", "dbh", "ci"))

  # check if types are correct
  if (!all(unique(data$type) %in% c("adult", "dead", "sapling", "seedling"))) {

    stop("The type of the individuals must be one of: 'adult', 'dead', 'sapling', 'seedling',",
         call. = FALSE)
  }

  # update type 1 < dbh <= 10
  data[dbh > 1 & dbh <= 10 & type != "dead", type := "sapling"]

  # update type below dbh > 10
  data[dbh > 10 & type != "dead", type := "adult"]

  return(data)
}
