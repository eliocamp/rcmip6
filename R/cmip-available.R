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
  files <- list.files(root, pattern = ".json", full.names = TRUE)
  info <- lapply(files, function(f) {
    data <- data.table::setDT(jsonlite::read_json(f,  simplifyVector = TRUE, flatten = TRUE))

    data[] <- lapply(data, function(d) {
      if (length(d[[1]]) == 1) {
        unlist(d)
      } else {
        d
      }
    })
    data$file <- file_from_info(data, root = root)
    data
  })

  data.table::rbindlist(info)
}

cmip_available_legacy <- function(root = cmip_root_get()) {
  infos <- list.files(root, pattern = "model.info",
                      recursive = TRUE, full.names = TRUE)

  data <- data.table::rbindlist(lapply(infos, function(i) {
    # Suppresses Calling 'structure(NULL, *)' is deprecated,
    # as NULL cannot have attributes
    suppressWarnings(jsonlite::unserializeJSON(readLines(i)))
  }))

  files <- lapply(infos, function(info) {
    list.files(dirname(info), pattern = ".nc",
               recursive = TRUE, full.names = TRUE)
  })

  data$files <- files

  return(data)
}
