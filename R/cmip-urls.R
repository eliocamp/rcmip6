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
  ## FIXME/MDSumner 1. note this function overlaps cmip_download_one() quite a lot
  ## 2. I'm not clear about the http vs https thing, seems to obtain a mix
  ## 3. Can this be way faster?
  op <- options(timeout = 360)
  on.exit(options(op), add = TRUE)
  vec <- rep(NA_character_, dim(results)[1L])
  for (i in seq_along(results)) {
    result <- results[i]
    if (is.na(result$index_node)) next;
    url <- paste0("https://", result$index_node, "/search_files/", result$id, "/", result$index_node, "/?limit=999")

    info <- httr::RETRY("GET", url = url)
    httr::warn_for_status(info)

    if (httr::http_error(info)) {
      next;
    }
    info <- try(httr::content(info)$response$docs)
    if (inherits(info, "try-error")) {
      next;
    }
    ## FIXME/MDSumner bit worried here about what the structure could be in here (can there be multiple?)
    #tx <- grep("HTTPServer", unlist(info[[1]]$url), value = TRUE)
    #vec[i] <- substr(tx, 1, gregexpr("\\.nc", tx)[[1]][1] + 2)
    vec[i] <-  strsplit(info[[1L]]$url[[1L]], "\\|")[[1L]][1L]
  }

  vec
}
