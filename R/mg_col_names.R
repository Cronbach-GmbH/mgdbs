#' Return column names of a mongo DB table (collection).
#'
#' @name mg_col_names
#'
#' @param table name (as character).
#'
#' @return character
mg_col_names <- function(table) {
  mongolite::mongo(table)$find(limit = 1) %>%  names()
}
