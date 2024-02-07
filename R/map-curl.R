# From https://github.com/noamross/scrapetools/blob/eb1be9de5db15997669e572cdd4be508919b8438/R/map_curl.R
map_curl <- function(urls, files = NULL, sizes = NA,
                     delay = 0.5, retry = 5,
                     total_connections = 100L,
                     host_connections = 6L) {
  out <- structure(vector("list", length(urls)), .Names = urls)
  attempts <- structure(rep(0, length(urls)), .Names = urls)

  # Make delay a bit random
  if (delay != 0) {
    delay_fn <- function() Sys.sleep(stats::rnorm(1, delay, delay/10)^2)
  } else {
    delay_fn <- function() {}
  }

  if (!is.null(files)) {
    stopifnot(length(urls) == length(files))
    names(files) <- urls
  }

  pb <- progress::progress_bar$new(total = sum(sizes),
                                   format = ":spin [:bar] :rate - :bytes (:percent)",
                                   clear = FALSE)

  make_handle <- function(i) {
    h <- curl::new_handle(url = urls[i])
    h <- curl::handle_setopt(h, .list = list(low_speed_limit = 100,
                                             low_speed_time = 30))
    return(h)
  }

  map_pool <- curl::new_pool(total_con = total_connections,
                             host_con = host_connections)

  done_fn <- function(resp, i) {
    resp$content <- NULL  # Remove content to free up memory.
    out[[i]] <<- resp
    attempts[[i]] <<- attempts[[i]] + 1
    if (!pb$finished) {
      pb$message(msg = tr_("Successfuly downloaded: %s", resp$url))
    }
    delay_fn()
  }

  fail_fn <- function(err, i) {
    message(tr_("Failed: %s with error %s", urls[i],  err))
    unlink(files[i])   # Clean up unfinished files.
    attempts[i] <<- attempts[i] + 1
    if (attempts[i] <= retry) {
      message(tr_("Retrying..."))
      # It would be great to add support for resuming the download.
      if (exists(files[i])) {
        pb$tick(len = -file.size(files[i]))  # remove downloaded data from progress
      }

      add_job(i)
      delay_fn()
    }
  }

  # This is not strictly needed, but it can be useful for debugging purposes
  # (Add some message or breakpoint)
  data_fn <- function(data, close, i, writer) {
    if (!close & !pb$finished) {
      pb$tick(len = length(data))
    }
    writer(data, close)
  }

  add_job <- function(i) {
    force(i)

    writer <- curl::file_writer(files[i], append = FALSE)
    curl::multi_add(make_handle(i),
                    done = function(resp) done_fn(resp, i),
                    fail = function(err) fail_fn(err, i),
                    data = function(data, close) data_fn(data, close, i, writer),
                    pool = map_pool)
  }

  for (i in seq_along(urls)) {
    add_job(i)
  }

  curl::multi_run(timeout = Inf, poll = FALSE, pool = map_pool)

  pb$terminate()
  if (!is.null(names(urls))) {
    names(out) <- names(urls)
    names(attempts) <- names(attempts)
  }
  attr(out, "attempts") <- attempts
  class(out) <- "map_curl"
  return(out)
}
