#' Mange ESGF nodes
#'
#' @param node URL of the node to use.
#'
#'
#' @examples
#' old_node <- cmip_node_set("https://esgf-node.ipsl.upmc.fr")
#'
#' # Restore old node
#' cmip_node_set(old_node)
#'
#'
#' @export
#' @rdname cmip_node
cmip_node_set <- function(node = "https://esgf-node.llnl.gov") {
  old <- cmip_node_get()
  options(cmip_node = node)
  return(invisible(old))
}

#' @export
#' @rdname cmip_node
cmip_node_get <- function() {
  getOption("cmip_node", default = "https://esgf-node.llnl.gov")
}
