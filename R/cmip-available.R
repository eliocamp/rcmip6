#' List downloaded models.
#'
#' Reads files in your local CMIP6 directory and parses the contents into a
#' data.frame
#'
#' @inheritParams cmip_download
#'
#' @details
#' `cmip_available_legacy()` reads from the older way of storing information
#' and will only work for files downloaded with version <= 0.0.2 of the package.
#' `cmip_available_convert()` will try to read your downloads and convert it
#' to the new store.
#'
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


#' @rdname cmip_available
#' @export
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

#' @rdname cmip_available
#' @export
cmip_available_convert <- function(root = cmip_root_get()) {
  available <- cmip_available_legacy(root)

  info <- available$info

  ## Info has almost the same information than results and it's used for storage
  ## Results do have some other columns, add them just in case.
  for (i in seq_along(info)) {
    missing_columns <- colnames(available)[!(colnames(available) %in% colnames(info[[i]]))]
    info[[i]][missing_columns] <- lapply(available[i, missing_columns, with = FALSE],
                                         rep, times = nrow(info[[i]]))
  }

  info <- flatten_info(info)

  file <- file.path(root, "legacy.json")
  jsonlite::write_json(info, file)
}

