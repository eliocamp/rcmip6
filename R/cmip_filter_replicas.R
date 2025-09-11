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
  nodes <- get_data_node()[online == TRUE]

  results <- results[data_node %in% nodes[["data_node"]]]
  results <- results[, .SD[1], by = instance_id]
  results
}


get_data_node <- function(timeout = 3) {
  status_url <- "https://aims2.llnl.gov/proxy/status"
  data <- jsonlite::read_json(status_url)$data$result

  data <- lapply(data, function(x) {
    data.table::data.table(
      data_node = x$metric$instance,
      online = x$value[[2]] == 1
    )
  })
  data.table::rbindlist(data)
}
