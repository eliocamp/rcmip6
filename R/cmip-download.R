#' Downloads CMIP data
#'
#' @param results A list of search results from [cmip_search()].
#' @param root Root folder to download and organise the data.
#' @param user,comment Optional strings to use when saving the log for each file.
#' @param ... Ignored
#'
#' @return
#' A list of files.
#'
#' @export
cmip_download <- function(results, root = cmip_root_get(), user = Sys.info()[["user"]], comment = NULL, ...) {
  root <- path.expand(root)

  if (inherits(results, "cmip_simple")) {
    results <- cmip_unsimplify(results)
  }

  lapply(seq_len(nrow(results)), function(i) {
    cmip_download_one(results[i, ], root = root, user = user, comment = comment, ...)
  })
}

cmip_download_one <- function(result,
                              root = cmip_root_get(),
                              user = Sys.info()[["user"]],
                              comment = NULL, ...) {
  dir <- result_dir(result, root = root)

  use_https <- list(...)[["use_https"]]
  # Some results have multiple folders? (CMIP5, seems like)
  sink <- lapply(dir, dir.create,  showWarnings = FALSE, recursive = TRUE)

  url <- paste0("https://", result$index_node, "/search_files/", result$id, "/", result$index_node, "/?limit=999")

  info <- httr::content(httr::GET(url))$response$docs

  files <-  vapply(info, function(i) {
    url <- strsplit(i$url[[1]], "\\|")[[1]][1]

    i$version <- result$version  # CMIP5 seems to have a different version in the file thing?
    file <- file.path(result_dir(i), i$title)
    message(glue::glue(tr_("Downloading {i$title}...")))
    if (file.exists(file)) {
      checksum_file <- paste0(file, ".chksum")
      checksum_type  <- tolower(i$checksum_type[[1]])
      if (file.exists(checksum_file)) {
        local_checksum <- readLines(checksum_file)
      } else {
        local_checksum <- digest::digest(file = file, algo = checksum_type)
        writeLines(text = local_checksum, con = checksum_file)
      }

      checksum <- i$checksum[[1]]

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
                            times = 10,
                            httr::write_disk(file, overwrite = TRUE),
                            httr::progress()), silent = TRUE)
    if (inherits("try-error", response))  {
      warning(response)
      return(NA_character_)
    }

    httr::warn_for_status(response, task = NULL)

    if (httr::http_error(response)) {
      return(NA_character_)
    }
    log <- paste(as.character(as.POSIXlt(Sys.time(), tz = "UTC")), "-", user)
    writeLines(c(log, comment), file.path(result_dir(i), paste0(tools::file_path_sans_ext(i$title), ".log")))
    file
  }, character(1))

  writeLines(jsonlite::serializeJSON(result, pretty = TRUE),
             file.path(dir, "model.info"),)
  files
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




