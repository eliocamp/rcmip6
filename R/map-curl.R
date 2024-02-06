# From https://github.com/noamross/scrapetools/blob/eb1be9de5db15997669e572cdd4be508919b8438/R/map_curl.R
map_curl <- function(urls, files = NULL, delay = 1, retry = 5,
                     total_connections = 100L, host_connections = 6L) {
  out <- structure(vector("list", length(urls)), .Names = urls)
  attempts <- structure(rep(0, length(urls)), .Names = urls)

  # Make delay a bit random
  if (delay != 0) {
    delay_fn <- function() Sys.sleep(rnorm(1, delay, delay/10)^2)
  } else {
    delay_fn <- function() {}
  }

  if (!is.null(files)) {
    stopifnot(length(urls) == length(files))
    names(files) <- urls
  }

  make_handle <- function(i) {
    h <- curl::new_handle(url = urls[i])
    h <- curl::handle_setopt(h, .list = list(low_speed_limit = 100,
                                             low_speed_time = 30))
    return(h)
  }

  map_pool <- curl::new_pool(total_con = total_connections,
                             host_con = host_connections)

  done_fn <- function(resp, i) {
    if (!is.null(files)) {
      # on.exit(print(i))
      con <- file(files[i], "wb")
      writeBin(resp$content, con)
      close.connection(con)
    }
    resp$content <- NULL  # Remove content to free up memory.

    out[[i]] <<- resp
    attempts[[i]] <<- attempts[[i]] + 1

    message(tr_("Successfuly downloaded: %s", resp$url))
    delay_fn()
  }

  fail_fn <- function(err, i) {
    message(tr_("Failed: %s with error %s", urls[i],  err))

    attempts[i] <<- attempts[i] + 1
    if (attempts[i] <= retry) {
      message(tr_("Retrying..."))
      curl::multi_add(make_handle(i),
                      done = done_fn,
                      pool = map_pool,
                      fail = function(err) fail_fn(err, i))
      delay_fn()
    }

  }

  for (i in seq_along(urls)) {
    fail_closure <- function() {
      ival <- i
      function(err) fail_fn(err, ival)
    }

    done_closure <- function() {
      ival <- i
      function(resp) done_fn(resp, ival)
    }


    curl::multi_add(make_handle(i),
                    done = done_closure(),
                    fail = fail_closure(),
                    pool = map_pool)
  }

  curl::multi_run(timeout = Inf, poll = FALSE, pool = map_pool)

  if (!is.null(names(urls))) {
    names(out) <- names(urls)
    names(attempts) <- names(attempts)
  }
  attr(out, "attempts") <- attempts
  class(out) <- "map_curl"
  return(out)
}
