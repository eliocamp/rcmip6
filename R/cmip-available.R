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
#'
#'
#' @returns
#' A data.frame.
#'
#' @export
cmip_available <- function(root = cmip_root_get()) {
  files <- list.files(root, pattern = ".json", full.names = TRUE)
  info <- lapply(files, function(f) {
    data <- data.table::setDT(jsonlite::read_json(
      f,
      simplifyVector = TRUE,
      flatten = TRUE
    ))

    for (d in seq_along(data)) {
      if (is.list(data[[d]]) && all(lengths(data[[d]]) == 1)) {
        data[[d]] <- unlist(data[[d]])
      }
    }

    data$file <- file_from_info(data, root = root)
    data
  })

  data.table::rbindlist(info, fill = TRUE)
}


#' @rdname cmip_available
#' @export
cmip_available_legacy <- function(root = cmip_root_get()) {
  infos <- list.files(
    root,
    pattern = "model.info",
    recursive = TRUE,
    full.names = TRUE
  )

  data <- data.table::rbindlist(lapply(infos, function(i) {
    # Suppresses Calling 'structure(NULL, *)' is deprecated,
    # as NULL cannot have attributes
    suppressWarnings(jsonlite::unserializeJSON(readLines(i)))
  }))

  files <- lapply(infos, function(info) {
    list.files(
      dirname(info),
      pattern = ".nc$",
      recursive = TRUE,
      full.names = TRUE
    )
  })

  data$files <- files

  return(data)
}


cmip_available_write <- function(
  results,
  root = cmip_root_get(),
  year_range = c(-Inf, Inf)
) {
  # Request metadata and check download necessity
  if (is.null(results$info)) {
    results <- cmip_add_info(results)
  }

  message(tr_("Checking for existing files..."))
  results <- cmip_add_needs_download(
    results,
    root = root,
    year_range = year_range
  )

  is_requested <- unlist(extract_info_column(results, "is_requested"))
  needs_download <- unlist(extract_info_column(results, "needs_download"))

  metadata_existing <- flatten_info(results$info)[
    is_requested & !needs_download
  ]

  file <- cmip_database_file(root = root)
  jsonlite::write_json(metadata_existing, file, pretty = TRUE)

  return(invisible(file))
}
