#' List downloaded models.
#'
#' Reads files in your local CMIP6 directory and parses the contents into a
#' data.frame
#'
#' @inheritParams cmip_download
#'
#' @returns
#' A data.frame.
#'
#' @export
cmip_available <- function(root = cmip_root_get()) {
  infos <- list.files(root, pattern = "model.info", recursive = TRUE, full.names = TRUE)

  data <- lapply(infos, jsonlite::read_json, simplifyVector = TRUE)

  files <- lapply(infos, function(info) {
    list.files(dirname(info), pattern = ".nc", recursive = TRUE, full.names = TRUE)
  })

  data <- cmip_parse_search(data)
  data$files <- files

  return(data)
}
