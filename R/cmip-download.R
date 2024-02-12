#' Downloads CMIP data
#'
#' @param results A list of search results from [cmip_search()].
#' @param root Root folder to download and organise the data.
#' @param year_range An integer vector of length 2, indicating the start and
#' end range of years. Restricts the download of model output with files that
#'  include some data within this range of years. Defaults to c(-Inf, Inf) to
#'  include all possible files
#' @param user,comment Deprecated.
#' @param check_diskspace Logical indicating whether to check if location has
#' enough space to download all the requested files.
#' @param download_config a list of arguments to configure the behaviour of
#' downloads.
#' @param ... Ignored
#'
#' @return
#' A list of files.
#'
#' @export
cmip_download <- function(results,
                          root = cmip_root_get(),
                          user = Sys.info()[["user"]],
                          comment = NULL,
                          year_range = c(-Inf, Inf),
                          check_diskspace = TRUE,
                          download_config = cmip_download_config(),
                          ...) {
  used_deprecated <- c(user = !missing(user),
                       comment = !missing(comment))
  if (any(used_deprecated)) {
    used_deprecated <- paste0(names(used_deprecated)[used_deprecated],
                              collapse = ", ")
    warning(tr_("%s have been deprecated and will be ignored.", used_deprecated))
  }


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

  message(tr_("Requesting metadata..."))
  # Request metadata and check download necessity
  if (is.null(results$info)) {
    results <- cmip_add_info(results)
  }

  message(tr_("Checking for existing files..."))
  results <- cmip_add_needs_download(results, root = root,
                                     year_range = year_range)

  # Should download?
  is_requested <- extract_info_column(results, "is_requested")
  needs_download <- extract_info_column(results, "needs_download")


  # These variables are lists of length nrow(requests)
  # I need to flatten the list to vectorise and paralellise
  # but then I want to pack the results back into the same structure
  # I also need to keep track of which files were downloaded
  pack_into <- vapply(is_requested, sum, FUN.VALUE = integer(1))
  ids <- rep(results$title, times = pack_into)

  is_requested <- unlist(is_requested)
  needs_download <- unlist(needs_download)

  # Get all URLS
  urls <- unlist(info_lapply(results, urls_from_info))

  # Get all filenames
  files <- unlist(info_lapply(results, file_from_info, root = root))
  file_size <- unlist(extract_info_column(results, "size"))

  if (sum(is_requested) == 0) {
    warning(tr_("No files within specified year_range."))
    return(character(0))
  }

  if (sum(needs_download) == 0) {
    message(tr_("All files already downloaded"))
    return(split(files[is_requested], ids))
  }

  if (check_diskspace) {
    available_disk <- ps::ps_disk_usage(root)$available
    if (sum(file_size) > available_disk) {
      stop(tr_(c("Not enough disk space. Need to download %s by %s avaiable.\n",
                 "Use `check_diskspace = FALSE` to skip this check."),
               format_bytes(sum(file_size)),
               format_bytes(available_disk)))
    }
  }


  if (sum(needs_download) != sum(is_requested)) {
    message(tr_("Skipping, %i files already downloaded.", sum(is_requested) - sum(needs_download)))
  }

  # Create all folders
  sink <- vapply(unique(dirname(files)[needs_download]),
                 dir.create,
                 FUN.VALUE = numeric(1),
                 recursive = TRUE, showWarnings = FALSE)
  metadata <- flatten_info(results$info)[needs_download]
  metadata$is_requested <- NULL
  metadata$needs_download <- NULL

  message(tr_("Downloading..."))
  downloaded <- map_curl(urls = urls[needs_download],
                         files = files[needs_download],
                         sizes = file_size[needs_download],
                         metadata = metadata,
                         database_file = cmip_database_file(root = root),
                         delay = download_config$delay,
                         retry = download_config$retry,
                         total_connections = download_config$total_connections,
                         host_connections = download_config$host_connections
                         )


  out <- split(files[is_requested], ids)
  attr(out, "status") <- downloaded
  # TODO: I can't do this with the new refactor.
  # # Some instances can fail in one replica but not others,
  # # so we have to list all instances and remove the
  # all_instances <- results[["instance_id"]]
  # downloaded_instances <- all_instances[downloaded]
  # failed_instances <- setdiff(all_instances, downloaded_instances)
  #
  # if (length(failed_instances) != 0) {
  #   warning("Failed to download some instances, query them with,\n",
  #   instance_query(failed_instances))
  # }
  # return(invisible(files))
  return(out)

}


#' @param delay delay in seconds between retries. The actual delay adds a bit
#' of randomness.
#' @param retry number of retries before giving up on a download.
#' @param total_connections maximum number of total concurrent connections.
#' @param host_connections maximum number concurrent connections per host.
#' @param low_speed_limit,low_speed_time the download will fail if it downloads
#' at below `low_speed_limit` bytes/second for more than `low_speed_time` seconds.
#'
#'
#' @rdname cmip_download
#' @export
cmip_download_config <- function(delay = 0.5,
                                 retry = 5,
                                 total_connections = 1,
                                 host_connections = 1,
                                 low_speed_limit = 100,
                                 low_speed_time = 30) {

  list(delay = delay,
       retry = retry,
       total_connections = total_connections,
       host_connections = host_connections,
       low_speed_limit = low_speed_limit,
       low_speed_time = low_speed_time)

}


filter_list <- function(list, conditions) {
  for (l in seq_along(list)) {
    list[[l]] <- list[[l]][conditions[[l]]]
  }
  return(list)
}

extract_info_column <- function(results, column) {
  lapply(seq_len(nrow(results)), function(i) {
    unlist(results[i, ]$info[[1]][[column]])
  })
}

info_lapply <- function(results, fun, ...) {
  fun <- match.fun(fun)
  lapply(seq_len(nrow(results)), function(i) {
    fun(results[i, ]$info[[1]], ...)
  })
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
                         .close = ")s")

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


cmip_database_file <- function(root = cmip_root_get()) {
  file <- paste0(format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), "_rcmip6.json")
  file.path(root, file)
}

cmip_database_write <- function(database, root = cmip_root_get()) {

  jsonlite::write_json(database, cmip_database_file)

}
cmip_database_read <- function(root = cmip_root_get()) {



}
