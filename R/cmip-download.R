#' Downloads CMIP data
#'
#' @param results A list of search results from [cmip_search()].
#' @param root Root folder to download and organise the data.
#' @param user,comment Optional strings to use when saving the log for each file.
#' @param ... Arguments passed to [utils::download.file()]
#'
#' @return
#' A list of files.
#'
#' @export
cmip_download <- function(results, root = cmip_root_get(), user = Sys.info()[["user"]], comment = NULL, ...) {
  root <- path.expand(root)
  lapply(results, cmip_download_one, root = root, user = user, comment = comment, ...)

}

cmip_download_one <- function(result,
                              root = cmip_root_get(),
                              user = Sys.info()[["user"]],
                              comment = NULL, ...) {
  dir <- result_dir(result, root = root)
  dir.create(dir, FALSE, TRUE)

  url <- paste0("https://", result$index_node, "/search_files/", result$id, "/", result$index_node, "/?limit=999")

  info <- httr::content(httr::GET(url))$response$docs

  jsonlite::write_json(result, file.path(dir, "model.info"), pretty = TRUE)

  files <-  vapply(info, function(i) {
    url <- strsplit(i$url[[1]], "\\|")[[1]][1]
    file <- file.path(dir, i$title)

    if (file.exists(file)) {
      checksum_type  <- tolower(i$checksum_type[[1]])
      checksum <- i$checksum[[1]]
      local_checksum <- digest::digest(file = file, algo = checksum_type)

      if (local_checksum == checksum) {
        message(tr_("Skipping existing file with matching checksum."))
        return(file)
      }
    }
    message(glue::glue(tr_("Downloading {i$title}...")))
    utils::download.file(url = url,
                         destfile = file, ...)

    log <- paste(as.character(as.POSIXlt(Sys.time(), tz = "UTC")), "-", user)
    writeLines(c(log, comment), file.path(dir, paste0(tools::file_path_sans_ext(i$title), ".log")))
    file
  }, character(1))
  files
}



result_dir <- function(result, root = cmip_root_get()) {
  glue::glue_data(result,
                  result[["directory_format_template_"]][[1]],
                  .open = "%(",
                  .close = ")s"
  )
}

result_filename <- function(result) {
  glue::glue_data(result,
                  result[["dataset_id_template_"]][[1]],
                  .open = "%(",
                  .close = ")s"
  )
}



#' Computes the total size of a search result in Mb.
#'
#' @inheritParams cmip_download
#' @export
cmip_size <- function(results) {
  res <- sum(vapply(results, function(r) r$size, FUN.VALUE = 1))/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}


#' @export
print.cmip_size <- function(x, ...) {
  cat(signif(x, 3), "Mb")
}




