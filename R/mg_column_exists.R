#' Check if a table (collection) exists in a mongo DB.
#'
#' @name mg_column_exists
#'
#' @param table name as a character
#' @param name of the column as a character
#'
#' @return logical
mg_column_exists <- function(table, name) {
  name %in% (mg_col_names(table))
}
