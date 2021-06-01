#' Check if a table (collection) exists in a mongo DB.
#'
#' @name mg_table_exists
#'
#' @param table name as a character
#'
#' @return logical
mg_table_exists <- function(table) {
  table %in% mongolite::mongo()$run('{"listCollections":1}')$cursor$firstBatch$name
}
