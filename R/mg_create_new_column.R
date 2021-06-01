#' Creates a new column in a mongo DB table.
#'
#' @name mg_create_new_column
#' @param table name of the table.
#' @param name name of the new colum.
#' @param value default value for the new column.
#'
#' @return TRUE (if it doesn't error)
mg_create_new_column <- function(table, name, value) {

  if(mg_table_exists(table)) {
    mongolite::mongo(table)$update('{}', paste0('{"$set":{"', name, '": ', value, '}}'), multiple = TRUE)
  }

  invisible(NULL)
}
