cmip_add_needs_download <- function(results, root = cmip_root_get(), year_range = c(-Inf, Inf)) {
  if (is.null(results$info)) {
    infos <- get_results_info(results)
  } else {
    infos <- results$info
  }

  infos <- furrr::future_map(infos, info_add_needs_download, root = root, year_range = year_range)

  results$info <- infos
  return(results)
}

info_add_needs_download <- function(info, root = cmip_root_get(), year_range = c(-Inf, Inf)) {
  overlaps <- year_range_overlaps(info$title, year_range = year_range)
  file <- file.path(result_dir(info, root = root), info$title)
  exists <- file.exists(file)

  matches_checksum <- vapply(seq_along(info$title), function(i) {
    if (overlaps[i] && exists[i]) {  # Only check if necessary.
      return(checksum_matches(file[i],
                              checksum_type = tolower(info$checksum_type[[i]]),
                              checksum = info$checksum[[i]]))
    } else {
      return(FALSE)
    }
  }, logical(1))

  info$needs_download <- overlaps & !matches_checksum
  info
}

