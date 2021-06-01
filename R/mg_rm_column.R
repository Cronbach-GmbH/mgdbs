#' Remove a column from a mongo DB table (collection).
#'
#' @name mg_rm_column
#'
#' @param table name as a character
#' @param name of the column as a character
#'
#' @return logical
mg_rm_column <- function(table, name) {
  stopifnot(name %in% (mg_col_names(table)))

  mongolite::mongo(table)$update('{}', paste0('{"$unset":{"', name, '": true}}'), multiple = TRUE)
}
