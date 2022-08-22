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

# From epwshiftr
get_data_node <- function (timeout = 3) {
  # read html page
  f <- tempfile()
  utils::download.file("https://esgf-node.llnl.gov/status/", f, "libcurl", quiet = TRUE)
  l <- readLines(f)

  # locate table
  l_s <- grep("<!--load block main-->", l, fixed = TRUE)

  if (!length(l_s)) stop("Internal Error: Failed to read data node table")

  l <- l[l_s:length(l)]
  l_s <- grep("<table>", l, fixed = TRUE)[1L]
  l_e <- grep("</table>", l, fixed = TRUE)[1L]

  if (!length(l_s) || !length(l_e)) stop("Internal Error: Failed to read data node table")

  l <- l[l_s:l_e]

  # extract nodes
  loc <- regexec("\\t<td>(.+)</td>", l)
  nodes <- vapply(seq_along(l), function (i) {
    if (all(loc[[i]][1] == -1L)) return(NA_character_)
    substr(l[i], loc[[i]][2], loc[[i]][2] + attr(loc[[i]], "match.length")[2] - 1L)
  }, NA_character_)
  nodes <- nodes[!is.na(nodes)]

  # extract status
  loc <- regexec('\\t\\t<font color="#\\S{6}"><b>(UP|DOWN)</b>', l)
  status <- vapply(seq_along(l), function (i) {
    if (all(loc[[i]][1] == -1L)) return(NA_character_)
    substr(l[i], loc[[i]][2], loc[[i]][2] + attr(loc[[i]], "match.length")[2] - 1L)
  }, NA_character_)
  status <- status[!is.na(status)]


  if (length(nodes) != length(status)) stop("Internal Error: Failed to read data node table")

  res <- data.table::data.table(data_node = nodes, online = status == "UP")

  res

}
