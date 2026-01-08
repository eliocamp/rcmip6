#' Query CMIP data
#'
#' @param query A list that defines the search parameters. Optionally, a character
#' vector of instances IDs to be searched.
#' @param url The URL of the JSON query to convert into a list. See details.
#'
#' @details
#' The easiest way to get a valid `query` list is to use the search portal at
#' ([https://esgf-node.llnl.gov/search/cmip6/](https://esgf-node.llnl.gov/search/cmip6/))
#' to make a search that approximates what you are looking for. Then, under the
#' number of results there's a link that reads "return results as JSON".
#' Copy that link and pass it to `[cmip_url_to_list()]`.
#' On RStudio you can also use the AddIn.
#'
#' @return
#' A data.table of results.
#'
#' @export
cmip_search <- function(query) {
  if (is.character(query)) {
    query <- list(
      id = query
    )
  }
  query$format <- "application/solr+json"
  query$limit <- "9999"
  query$offset <- "0"
  # query$type <- "File"

  query <- lapply(query, function(q) {
    paste0(q, collapse = ",")
  })

  node <- cmip_node_get()
  url <- paste0(node, "/esg-search/search")

  response <- httr::GET(
    url,
    query = query
  )

  if (httr::http_error(response)) {
    stop(glue::glue(tr_("Search failed with status {response$status}.")))
  }

  search_results <- jsonlite::parse_json(
    httr::content(response, encoding = "UTF-8", as = "text"),
    simplifyVector = TRUE
  )

  search_results <- data.table::as.data.table(search_results$response$docs)
  columns_to_vector(search_results)[]
}

columns_to_vector <- function(results) {
  to_vector <- c(
    "activity_drs",
    "cf_standard_name",
    "citation_url",
    "data_specs_version",
    "dataset_id_template_",
    "directory_format_template_",
    "experiment_id",
    "experiment_title",
    "frequency",
    "further_info_url",
    "grid",
    "grid_label",
    "institution_id",
    "member_id",
    "mip_era",
    "model_cohort",
    "nominal_resolution",
    "pid",
    "product",
    "project",
    # "realm",
    "source_id",
    "sub_experiment_id",
    "table_id",
    "variable",
    "variable_id",
    "variable_long_name",
    "variable_units",
    "variant_label",
    "geo_units",
    "branch_method",
    "short_description"
  )

  for (col in to_vector) {
    vec <- results[[col]]
    vec[vapply(vec, is.null, logical(1))] <- NA

    data.table::set(results, NULL, col, unlist(vec))
  }
  results
}


#' @inheritParams cmip_download
#' @export
#' @rdname cmip_search
cmip_info <- function(results) {
  N <- .N <- .SD <- instance_id <- NULL
  results_filter <- results[, .SD[1], by = instance_id]

  n_replicas <- nrow(results[, .N, by = instance_id][N > 1])

  string <- tr_(
    "Found {nrow(results_filter)} results (with {n_replicas} replicas) totalling {round(cmip_size(results_filter))}Mb."
  )

  glue::glue(string)
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

list_pretty_format <- function(list) {
  max_n <- max(vapply(names(list), nchar, 0))

  elements <- lapply(seq_along(list), function(i) {
    paste0(
      "  ",
      formatC(names(list)[i], width = -max_n, flag = " "),
      " = \"",
      list[[i]],
      "\""
    )
  })

  paste0(
    "query <- list(\n",
    paste0(unlist(elements), collapse = ",\n"),
    "\n)\n"
  )
}


#' Simplifies the output of searches
#'
#' Removes a lot of less important columns from the output of
#' `cmip_search()`. The full dataset is stored in the hidden column
#' `full_info`.
#' Use `cmip_unsimplify()` to return to the original format
#'
#' @inheritParams cmip_download
#' @param data A simplifided output from `cmip_simplify()`
#'
#' @export
cmip_simplify <- function(results) {
  cmip6_folder_template <- gsub("%\\(", "", cmip6_folder_template)
  cmip6_folder_template <- gsub("\\)s", "", cmip6_folder_template)
  columns <- colnames(results)

  vars <- c(
    strsplit(cmip6_folder_template, "/")[[1]],
    "variable_long_name",
    "datetime_start",
    "datetime_stop",
    "nominal_resolution"
  )

  vars <- setdiff(vars, "root")
  simple <- results[, vars, with = FALSE]
  simple$full_info <- split(
    results[, -vars, with = FALSE],
    seq_len(nrow(results))
  )
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
  data <- cbind(data.table::copy(data)[, full_info := NULL], full_info)
  data[, columns, with = FALSE]
}


#' @export
print.cmip_simple <- function(x, ...) {
  full_info <- NULL
  x <- data.table::copy(x)[, full_info := NULL][]
  NextMethod("print")
}


.datatable.aware <- TRUE
