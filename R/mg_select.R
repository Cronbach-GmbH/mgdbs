#' Select columns from a mongo DB table
#'
#' @name mg_select
#' @param table name of the table/collection (as character).
#' @param ... column names (as symbols)
#' @param .query todo
#' @param .sort todo
#' @param .skip todo
#' @param .limit todo
#' @param .handler todo
#' @param .pagesize todo
#' @param ._id Should the _id column be returned?
#' @param .db todo (see documentation of mongolite::mongo)
#' @param .url todo (see documentation of mongolite::mongo)
#' @param .verbose todo (see documentation of mongolite::mongo)
#' @param .options todo (see documentation of mongolite::mongo)
#'
#' @return A data frame with the selected columns
mg_select <- function(table, ..., .query = "{}", .sort = "{}",
                      .skip = 0, .limit = 0, .handler = NULL,
                      .pagesize = 1000, ._id = FALSE,
                      .db = test, .url = "mongodb://localhost",
                      .verbose = FALSE, .options = mongolite::ssl_options()) {

  .mongo_collection <- mongolite::mongo(table)

  # Get the field names
  fields <- mg_col_names(table)

  field_list <- vector("list", length(fields))
  names(field_list) <- fields
  field_list <- list2DF(field_list)

  fields <- field_list %>% dplyr::select(...) %>% names()

  if (._id) {unique(c(fields, "_id"))}

  # Construct the fields-query
  if (._id) {
    fields <- paste0("{",
                     paste(paste0("\"", fields, "\""), 1, sep = ":", collapse = ","),
                     "}")
  }

  if (!._id & length(fields)) {
    fields <- paste0("{",
                     paste(paste0("\"", fields, "\""), 1, sep = ":", collapse = ","),
                     ",\"_id\":0}")
  }

  if (!length(fields)) {
    fields <- .mongo_collection$find(limit = 1, fields = '{}') %>%
      names()

    fields <- paste0("{",
                     paste(paste0("\"", fields, "\""), 0, sep = ":", collapse = ","),
                     "}")
  }

  # Run find on mongo_collection
  .mongo_collection$find(query = .query, sort = .sort, fields = fields,
                         skip = .skip, limit = .limit, handler = .handler, pagesize = .pagesize) %>%
    tibble::as_tibble()
}
