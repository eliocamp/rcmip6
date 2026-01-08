#' Filters replicas based on online servers
#'
#' Checks the online status of data nodes and returns filtered results
#' so that there is a single replica of each instance that is hosted
#' on an online node.
#'
#' @inheritParams cmip_download
#'
#' @return
#' A data.table.
#'
#' @export
cmip_filter_replicas <- function(results) {
  instance_id <- .SD <- data_node <- online <- NULL

  results <- results[is_online(data_node)]
  results <- results[, .SD[1], by = instance_id]
  results
}


is_online <- function(nodes) {
  nodes_unique <- unique(nodes)
  online <- vapply(nodes_unique, is_online_one, logical(1))
  online[nodes]
}

is_online_one <- function(node) {
  # Some nodes just fail to resolve and they throw an error
  response <- try(httr::HEAD(node), silent = TRUE)

  if (inherits(response, "try-error")) {
    return(FALSE)
  }

  identical(httr::status_code(response), 200L)
}
