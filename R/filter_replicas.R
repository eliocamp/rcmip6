filter_replicas <- function(resutls) {
  nodes <- get_data_node()

  results <- results[data_node %in% nodes[status == "UP"]$data_node]
  results <- results[, .SD[1], by = .(instance_id)]
}

# From epwshiftr
get_data_node <- function (timeout = 3) {
  # read html page
  f <- tempfile()
  utils::download.file("https://esgf-node.llnl.gov/status/", f, "libcurl", quiet = TRUE)
  l <- readLines(f)

  # locate table
  l_s <- grep("<!--load block main-->", l, fixed = TRUE)
  # nocov start
  if (!length(l_s)) stop("Internal Error: Failed to read data node table")
  # nocov end
  l <- l[l_s:length(l)]
  l_s <- grep("<table>", l, fixed = TRUE)[1L]
  l_e <- grep("</table>", l, fixed = TRUE)[1L]
  # nocov start
  if (!length(l_s) || !length(l_e)) stop("Internal Error: Failed to read data node table")
  # nocov end
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

  # nocov start
  if (length(nodes) != length(status)) stop("Internal Error: Failed to read data node table")
  # nocov end
  res <- data.table::data.table(data_node = nodes, status = status)
  data.table::setorderv(res, "status", -1)

  res

}
