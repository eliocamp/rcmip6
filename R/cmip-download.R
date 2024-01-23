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

  files <- lapply(seq_len(nrow(results)), function(i) {
    cmip_download_one(results[i, ], root = root, user = user, comment = comment,  year_range = year_range, ...)
  })

  downloaded <- vapply(files, function(x) all(!is.na(x)), logical(1))

  # Some instances can fail in one replica but not others,
  # so we have to list all instances and remove the
  all_instances <- results[["instance_id"]]
  downloaded_instances <- all_instances[downloaded]
  failed_instances <- setdiff(all_instances, downloaded_instances)

  if (length(failed_instances) != 0) {
    warning("Failed to download some instances, query them with,\n", instance_query(failed_instances))
  }
  return(invisible(files))

}


instance_query <- function(x) {
  start <- "cmip_search(c(\""
  space <- paste0(", \n", paste0(rep(" ", nchar(start)), collapse = ""))
  x <- paste(x, collapse = space)

  paste0(start, x, "\"))")
}

cmip_download_one <- function(result, root = cmip_root_get(), user = Sys.info()[["user"]], comment = NULL,  year_range = year_range, ...) {
  dir <- result_dir(result, root = root)
  use_https <- list(...)[["use_https"]]
  # Some results have multiple folders? (CMIP5, seems like)
  sink <- lapply(dir, dir.create,  showWarnings = FALSE, recursive = TRUE)
  info <- get_result_info(result)

  files <-  vapply(seq(1, nrow(info)), function(i) {
    url <- strsplit(info[i, ]$url[[1]], "\\|")[[1]][1]

    # The date is in a format of 6 digits separated by a dash or underscore
    # Capture the first four digits of each group of digits.
    date_range_regex <- "(\\d{4})\\d{2}[-_](\\d{4})\\d{2}"
    dates <- utils::strcapture(date_range_regex, info[i, ]$title,
                               proto = list(file_date_start = integer(),
                                            file_date_end = integer()))
    file_date_start <- dates$file_date_start
    file_date_end <- dates$file_date_end

    if (any(is.na(c(file_date_start, file_date_end)))) {
      warning(tr_("Failed to parse dates. Downloading anyway."))
    } else {

      # Get the intersections of windows specified by user and dates contained in the file
      # These statements could be nested, but are not too expensive anyway
      # Is the file fully inside the window?
      file_inside_window <- (year_range[1] <= file_date_start) & (file_date_end <= year_range[2])
      # Is the window specified by the user within the file?
      window_inside_file <- (file_date_start <= year_range[1]) & (file_date_end >= year_range[2])
      # Does the window intersect the start of the file?
      left <- (year_range[1] <= file_date_start) & (file_date_start <= year_range[2])
      # Does the window intersect the end of the file?
      right <- (year_range[1] <= file_date_end) & (file_date_end <= year_range[2])
      # Does the window partially contain the file?
      file_touches_window <- left | right

      if (!any(file_touches_window, file_inside_window, window_inside_file)) {
        message(tr_("Not downloading (file is not within specified dates.)"))
        return(NA_character_)
      }
    }



    info[i, ]$version <- result$version  # CMIP5 seems to have a different version in the file thing?
    file <- file.path(result_dir(info[i, ]), info[i, ]$title)
    message(glue::glue(tr_("Downloading {info[i, ]$title}...")))
    checksum_file <- paste0(file, ".chksum")
    checksum_type  <- tolower(info[i, ]$checksum_type[[1]])


    if (file.exists(file)) {
      if (file.exists(checksum_file)) {
        local_checksum <- readLines(checksum_file)
      } else {
        local_checksum <- digest::digest(file = file, algo = checksum_type)
        writeLines(text = local_checksum, con = checksum_file)
      }

      checksum <- info[i, ]$checksum[[1]]

      if (local_checksum == checksum) {
        message(tr_("Skipping (matching checksum)."))
        return(file)
      }
    }

    # Workaround para evitar el proxy de mierda de la UBA
    if (isTRUE(use_https)) {
      url <- gsub("http:", "https:", url)
    }

    message(glue::glue(tr_("Downloading from {url}")))
    response <- try(httr::RETRY("GET", url = url,
                                times = 3,
                                httr::write_disk(file, overwrite = TRUE),
                                httr::progress()))

    # RETRY will raise a stop() if the last try is a curl error
    # so we need to capture it.
    if (inherits(response, "try-error"))  {
      warning(response)
      unlink(file)
      return(NA_character_)
    }

    # Trap http errors
    if (httr::http_error(response)) {
      httr::warn_for_status(response, task = NULL)
      unlink(file)
      return(NA_character_)
    }

    # Compute and save checksum. Perhaps we need to also check if it's correct, but
    # what should we do if it's not?
    local_checksum <- digest::digest(file = file, algo = checksum_type)
    writeLines(text = local_checksum, con = checksum_file)

    log <- paste(as.character(as.POSIXlt(Sys.time(), tz = "UTC")), "-", user)
    writeLines(c(log, comment), file.path(result_dir(info[i, ]), paste0(tools::file_path_sans_ext(info[i, ]$title), ".log")))
    file
  }, character(1))


  if (any(is.na(files))) {
    result[["complete_download"]] <- FALSE
  } else {
    result[["complete_download"]] <- TRUE
  }

  if (any(!is.na(files))) {
    writeLines(jsonlite::serializeJSON(result, pretty = TRUE),
               file.path(dir, "model.info"),)
  }

  return(files)
}


result_dir <- function(result, root = cmip_root_get()) {

  template <- result[["directory_format_template_"]][[1]]
  if (is.null(template)) {
    # This is CMIP5
    template <- cmip5_folder_template
  }

  dir <- glue::glue_data(result,
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




