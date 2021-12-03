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
#'
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
  parsed <- lapply(results, function(result) {
    datetime_start <- result$datetime_start
    if(length(datetime_start) == 0) datetime_start <- NA

    datetime_stop <- result$datetime_stop
    if(length(datetime_stop) == 0) datetime_stop <- NA

    data <- unglue::unglue_data(result[["title"]],
                                .pattern_python_to_r(result[["dataset_id_template_"]][[1]]))
    member <- parse_member_id(data$member_id)

    data.frame(
      mip_era = result[["mip_era"]][[1]],
      institution_id = result[["institution_id"]][[1]],
      source_id = result$source_id[[1]],
      experiment_id = result$experiment_id[[1]],
      sub_experiment_id = result[["sub_experiment_id"]][[1]],
      experiment_title = result[["experiment_title"]][[1]],
      member_id = result[["member_id"]][[1]],
      realization_index = member$realization_index,
      initialization_index = member$initialization_index,
      physics_index = member$physics_index,
      forcing_index = member$forcing_index,
      table_id = result$table_id[[1]],
      frequency =  result$frequency[[1]],
      datetime_start = datetime_start,
      datetime_stop = datetime_stop,
      variable_id = result$variable_id[[1]],
      nominal_resolution = result$nominal_resolution[[1]],
      grid_label = result$grid_label[[1]],
      size = result$size/1024/1024
    )
  })

  parsed <- do.call(rbind, parsed)
  parsed
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

