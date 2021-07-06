#' Select columns from a mongo DB table employing the iterate method.
#'
#' @name mg_select2
#' @param table name of the table/collection (as character).
#' @param ... column names (as symbols)
#' @param .query todo
#' @param .sort todo
#' @param .skip todo
#' @param .limit todo
#' @param .handler todo
#' @param .pagesize todo
#' @param ._id Should the _id column be returned?
#'
#' @return A data frame with the selected columns
mg_select2 <- function(table, ..., .query = "{}", .sort = "{}",
                      .skip = 0, .limit = 0, .handler = NULL,
                      .pagesize = 1000, ._id = FALSE) {

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
  # .mongo_collection$find(query = .query, sort = .sort, fields = fields,
  #                        skip = .skip, limit = .limit, handler = .handler, pagesize = .pagesize) %>%
  #   tibble::as_tibble()
  it <- .mongo_collection$iterate(query = .query, sort = .sort, fields = fields,
                         skip = .skip, limit = .limit) 
  
  # res = c() 
  # try(
  #   repeat({
  #     res = dplyr::bind_rows(res, it$page(size=10000))
  #   }),
  #   silent=TRUE
  # )
  # 
  # # dplyr::bind_rows(it$batch())
  # res
}


# while(!is.null(res <- bla2$batch(size = 10000))) {
#   chunk <- data.table::rbindlist(res)
#   }
# 
# 
# bla <- mg_select("psych_prod_1",
#           .query = paste0("{\"meta_article_id\": {\"$gte\": ",
#                           12000001, ", \"$lte\": ", 12250000, "}}"),
#           meta_article_id,
#           meta_url,
#           meta_date_time,
#           meta_published_date,
#           meta_n,
#           meta_paywall,
#           meta_mobile_url,
#           meta_last_updated,
#           
#           starts_with("score_"))
# bla2 <- mg_select2("psych_prod_1",
#           .query = paste0("{\"meta_article_id\": {\"$gte\": ",
#                           12000001, ", \"$lte\": ", 12250000, "}}"),
#           meta_article_id,
#           meta_url,
#           meta_date_time,
#           meta_published_date,
#           meta_n,
#           meta_paywall,
#           meta_mobile_url,
#           meta_last_updated,
#           
#           starts_with("score_"))

# for (i in 1:100000) {print(identical(names(bla2$one()), names(bla2$one())))}