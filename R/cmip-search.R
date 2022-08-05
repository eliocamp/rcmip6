#' Query CMIP data
#'
#' @param query A list that defines the search parameters.
#' @param url The URL of the JSON query to convert into a list. See details.
#'
#' @details
#' The easiest way to get a valid `query` list is to use the search portal at
#' ([https://esgf-node.llnl.gov/search/cmip6/](https://esgf-node.llnl.gov/search/cmip6/))
#' to make a search that approximates what you are looking for. Then, under the number of
#' results there's a link that reads "return results as JSON". Copy that link and pass it
#' to `[cmip_url_to_list()]`.
#' On RStudio you can also use the AddIn.
#'
#' @return
#' A data.table of results.
#'
#' @export
cmip_search <- function(query) {
  query$format  <- "application/solr+json"
  query$limit   <- "9999"
  query$offset  <- "0"
  query$replica <- "false"

  query <- lapply(query, function(q) {
    paste0(q, collapse = ",")
  })

  search_results <- jsonlite::parse_json(httr::content(httr::GET("https://esgf-node.llnl.gov/esg-search/search",
                                                                 query = query)),
                                         simplifyVector = TRUE)
  search_results <- data.table::as.data.table(search_results$response$docs)
  search_results
}

#' @inheritParams cmip_download
#' @export
#' @rdname cmip_search
cmip_info <- function(results) {
  cat(glue::glue(tr_("Found {nrow(results)} results totalling {round(cmip_size(results))}Mb.")))
}


#' @rdname cmip_search
#' @export
cmip_url_to_list <- function(url) {
  query <- httr::parse_url(url)$query
  no_query <- c("offset", "limit", "facets", "format")
  query <- query[!(names(query) %in% no_query)]
  query <- lapply(query, function(q) gsub("\\+", " ", q))

  return(query)
}

#' Simplifies the output of searches
#'
#' Removes a lot of less important columns from the output of
#' `cmip_search()`. The full dataset is stored in the hidden column
#' `full_info`.
#' Use `cmip_unsimplify()` to return to the original format (this is needed fr )
#'
#' @inheritParams cmip_download
#' @param data A simplifided output from `cmip_simplify()`
#'
#' @export
cmip_simplify <- function(results) {
  cmip6_folder_template <- gsub("%\\(", "", cmip6_folder_template)
  cmip6_folder_template <- gsub("\\)s", "", cmip6_folder_template)
  columns <- colnames(results)

  vars <- c(strsplit(cmip6_folder_template, "/")[[1]],
            "variable_long_name",
            "datetime_start",
            "datetime_stop",
            "nominal_resolution")

  vars <- setdiff(vars, "root")
  simple <- results[, vars, with = FALSE]
  simple$full_info <- split(results[, -vars, with = FALSE], seq_len(nrow(results)))
  class(simple) <- c("cmip_simple", class(simple))
  attr(simple, "column") <- columns
  simple
}

#' @export
#' @rdname cmip_simplify
cmip_unsimplify <- function(data) {
  full_info <- NULL
  columns <- attr(data, "column", exact = TRUE)
  full_info <- data.table::rbindlist(data$full_info)
  data <- cbind(data.table::copy(data)[, full_info := NULL],
        full_info)
  data[, columns, with = FALSE]
}


#' @export
print.cmip_simple <- function(x, ...) {
  full_info <- NULL
  x <- data.table::copy(x)[, full_info := NULL]
  NextMethod("print")
}


.datatable.aware <- TRUE
