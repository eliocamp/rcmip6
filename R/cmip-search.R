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
#' to `cmip_url_to_list()`.
#' On RStudio you can also use the AddIn.
#'
#' @return
#' A list with search results. This can be parsed with [as.data.frame()] for better inspection.
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
                                                                 query = query)) )
  search_results <- search_results$response$docs
  class(search_results) <- c("cmip_results", class(search_results))
  search_results
}

#' @export
print.cmip_results <- function(x, ...) {
  cat(glue::glue(tr_("Found {length(x)} results totalling {round(cmip_size(x))}Mb.")))
}


#' @rdname cmip_search
#' @export
cmip_url_to_list <- function(url) {
  query <- httr::parse_url(url)$query
  no_query <- c("offset", "limit", "facets", "format")
  query <- query[!(names(query) %in% no_query)]

  return(query)
}

cmip_parse_search <- function(results) {

  cmip6_folder_template <- gsub("%\\(", "", cmip6_folder_template)
  cmip6_folder_template <- gsub("\\)s", "", cmip6_folder_template)
  vars <- c(strsplit(cmip6_folder_template, "/")[[1]],
            "variable_long_name",
            "datetime_start",
            "datetime_stop",
            "nominal_resolution")

  vars <- setdiff(vars, "root")

  a <- lapply(results, function(result) {
    data <- lapply(result[vars], function(x) {
      if (is.null(x)) {
        return(NA)
      }
      unlist(x)
    })
    names(data) <- vars
    as.data.frame(data, stringsAsFactors = FALSE)
    # data$full_info <- list(result)
    data
  })

  data <- Reduce(rbind,
                 lapply(results, function(result) {
                   data <- lapply(result[vars], function(x) {
                     if (is.null(x)) {
                       return(NA)
                     }
                     unlist(x)
                   })
                   names(data) <- vars
                   data <- as.data.frame(data, stringsAsFactors = FALSE)
                   data$full_info <- list(result)
                   data
                 })
  )

  data
}


parse_member_id <- function(member_id) {
  data <- unglue::unglue_data(member_id,
                              c("{sub_experiment_id}-r{realization_index}i{initialization_index}p{physics_index}f{forcing_index}",
                                "r{realization_index}i{initialization_index}p{physics_index}f{forcing_index}",
                                "{sub_experiment_id}-i{initialization_index}p{physics_index}f{forcing_index}",
                                "i{initialization_index}p{physics_index}f{forcing_index}"))
  if (!("sub_experiment_id" %in% colnames(data))) {
    data[["sub_experiment_id"]] <- rep("none", nrow(data))
  }
  data[["sub_experiment_id"]] <- replace(data[["sub_experiment_id"]], is.na(data[["sub_experiment_id"]]), "none")

  if (!("realization_index" %in% colnames(data))) {
    data[["realization_index"]] <- rep(NA, nrow(data))
  }

  data
}

#' @export
as.data.frame.cmip_results <- function(x, ...) {
  cmip_parse_search(x)
}

