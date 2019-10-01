#' @name rabmp
#' @docType package
#' @useDynLib rabmp
#' @importFrom Rcpp sourceCpp
# nocov start
"_PACKAGE"

#' Data table
#'
#' See \code{\link{set}} for details.
#'
#' @name :=
#' @rdname set
#' @keywords internal
#' @export
#' @importFrom data.table :=
NULL

globalVariables(c(
  "abiotic",
  "ci",
  "dbh",
  "i",
  "id",
  "..select_cols",
  "type",
  "x",
  "y",
  ".",
  ".."
  ))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE
