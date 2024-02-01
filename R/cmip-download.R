#' Downloads CMIP data
#'
#' @param results A list of search results from [cmip_search()].
#' @param root Root folder to download and organise the data.
#' @param year_range An integer vector of length 2, indicating the start and end range of years. Restricts the download of model output with files that include some data within this range of years. Defaults to c(-Inf, Inf) to include all possible files
#' @param user,comment Optional strings to use when saving the log for each file.
#' @param ... Ignored
#'
#' @return
#' A list of files.
#'
#' @export
cmip_download <- function(results, root = cmip_root_get(), user = Sys.info()[["user"]], comment = NULL, year_range = c(-Inf, Inf), ...) {
  if(year_range[1] > year_range[2]) {
    stop(tr_("The start year cannot be greater than the end year"))
  }

  root <- path.expand(root)

  # Evaluate these now so that if they involve expressions that can fail,
  # they fail early.
  force(user)
  force(comment)

  if (inherits(results, "cmip_simple")) {
    results <- cmip_unsimplify(results)
  }

  # Request metadata and check download necesity
  results <- cmip_add_needs_download(results, root = root, year_range = year_range)

  # Should download?
  needs_download <- unlist(lapply(seq_len(nrow(results)), function(i) unlist(results[i, ]$info[[1]]$needs_download)))

  # Get all URLS
  urls <- lapply(seq_len(nrow(results)), function(i) urls_from_info(results[i, ]$info[[1]]))
  urls <- unlist(urls)[needs_download]

  # Get all filenames
  files <- lapply(seq_len(nrow(results)), function(i) file_from_info(results[i, ]$info[[1]]))
  files <- unlist(files)[needs_download]

  checksums <- unlist(lapply(seq_len(nrow(results)), function(i) unlist(results[i, ]$info[[1]]$checksum)))[needs_download]
  checksum_types <- unlist(lapply(seq_len(nrow(results)), function(i) unlist(results[i, ]$info[[1]]$checksum_type)))[needs_download]
  checksum_files <- paste0(files, ".chksum")

  # Create all folders
  sink <- vapply(dirname(files), dir.create, FUN.VALUE = numeric(1), recursive = TRUE, showWarnings = FALSE)

  # Download all the stuff
  downloaded <- curl::multi_download(urls, files)

  # This creates all the checksums
  sink <- lapply(seq_along(files), function(i) {
    local_checksum <- digest::digest(file = files[i], algo = tolower(checksum_types[i]))
    writeLines(text = local_checksum, con = checksum_files[i])
    return(0)
  })

  # Write the model.info thing
  for (i in seq_len(nrow(results))) {
    these_files <- file_from_info(results[i, ]$info[[1]], root = root)
    they_exist <- file.exists(these_files)
    results[i, ]$info[[1]] <- results[i, ]$info[[1]][they_exist, ]

    writeLines(jsonlite::serializeJSON(results[i, ], pretty = TRUE),
               file.path(dirname(these_files)[1], "model.info"))
  }


  # TODO: I can't do this with the new refactor.
  # # Some instances can fail in one replica but not others,
  # # so we have to list all instances and remove the
  # all_instances <- results[["instance_id"]]
  # downloaded_instances <- all_instances[downloaded]
  # failed_instances <- setdiff(all_instances, downloaded_instances)
  #
  # if (length(failed_instances) != 0) {
  #   warning("Failed to download some instances, query them with,\n", instance_query(failed_instances))
  # }
  # return(invisible(files))
  return(downloaded)

}


instance_query <- function(x) {
  start <- "cmip_search(c(\""
  space <- paste0(", \n", paste0(rep(" ", nchar(start)), collapse = ""))
  x <- paste(x, collapse = space)

  paste0(start, x, "\"))")
}

result_dir <- function(info, root = cmip_root_get()) {

  template <- info$directory_format_template_[[1]]
  if (is.null(template)) {
    # This is CMIP5
    template <- cmip5_folder_template
  }

  dir <- glue::glue_data(info,
                         template,
                         .open = "%(",
                         .close = ")s"
  )

  dir
}



#' Computes the total size of a search result in Mb.
#'
#' @inheritParams cmip_download
#' @export
cmip_size <- function(results) {
  res <- sum(results$size)/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}


#' @export
print.cmip_size <- function(x, ...) {
  cat(signif(x, 3), "Mb")
}




