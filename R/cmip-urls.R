#' Obtain file server URLs of CMIP data
#'
#' Finds the URLs that [cmip_download] would obtain from a search results
#' object.
#'
#' If a valid URL is not found the return value has an `NA` in its place.
#'
#' Note that the versions to download have "/fileServer/" in the path, whereas
#' those for the remote access have "/dodsC/" instead (and can be
#' string-substituted).  This works for some sources we have seen (GDAL for
#' example can within limitations read remotely from the 'fileServer' URLs, but
#' the NetCDF library needs the 'dodsC' version, again this a pattern observed
#' on some sources not an authorative claim).
#'
#' It takes some time to process and find these so they aren't automatically
#' added to the results from a search.
#'
#' @inheritParams cmip_download
#'
#' @return character vector of URLs
#' @export
cmip_urls <- function(results) {
  infos <- get_results_info(results)
  lapply(infos, urls_from_info)
}


get_results_info <- function(results) {
  urls <- paste0("https://aims2.llnl.gov/metagrid-backend/proxy/search?dataset_id=",
                 utils::URLencode(results$id),
                 "&format=application%2Fsolr%2Bjson&limit=9999&offset=0&type=File&")

  files <- file.path(tempdir(), make.unique(results$title))

  res <- multi_download_retry(urls = urls, destfiles = files)

  if (any(res$status_code) != 200) {
    browser()
    warning(tr_("Failed to get metadata of some results. Ignoring them"))
  }

  results <- results[res$status_code == 200, ]
  res <- res[res$status_code == 200, ]
  lapply(res$destfile, function(file) {
    jsonlite::fromJSON(readLines(file))$response$docs
  })
}


multi_download_retry <- function(urls, destfiles, retry = 5) {
  # Not very elegant, but will have to do
  res <- curl::multi_download(urls = urls, destfiles = destfiles)

  to_retry <- res[res$status_code != 200, ]
  tries <- 1
  while(tries < retry & nrow(to_retry) > 0) {
    browser()
    res[res$status_code != 200, ] <- curl::multi_download(urls = to_retry$url, destfiles = to_retry$destfile)
    to_retry <- res[res$status_code != 200, ]
    tries <- tries + 1
  }

  return(res)
}

urls_from_info <- function(info) {
  vapply(info$url, function(x) strsplit(x[1], "\\|")[[1]][1], character(1))
}

file_from_info <- function(info, root = cmip_root_get()) {
  file.path(result_dir(info, root = root), info$title)
}
