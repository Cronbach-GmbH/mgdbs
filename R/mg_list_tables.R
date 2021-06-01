#' List all tables in a mongo DB.
#'
#' @name mg_list_tables
#'
#' @return character containing all table names in the DB.
mg_list_tables <- function() {

  out <- mongolite::mongo()$run('{"listCollections":1}')$cursor$firstBatch$name

  if(is.null(out)) {return(character(0))}

  out
}
