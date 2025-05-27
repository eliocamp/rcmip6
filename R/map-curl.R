
# From https://github.com/noamross/scrapetools/blob/eb1be9de5db15997669e572cdd4be508919b8438/R/map_curl.R
map_curl <- function(urls, files = NULL, sizes = NA,
                     metadata = NA,
                     database_file = NA,
                     delay = 0.5, retry = 5,
                     total_connections = 6L,
                     host_connections = 6L,
                     low_speed_limit = 100,
                     low_speed_time = 30,
                     options = NULL) {
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

  message(tr_("Downloading %s in %i files...", format_bytes(sum(sizes)), length(files)))
  pb <- progress::progress_bar$new(total = sum(sizes),
                                   format = ":spin [:bar] :rate - :bytes (:percent)",
                                   clear = FALSE)

  make_handle <- function(i) {
    h <- curl::new_handle(url = urls[i])
    options <- utils::modifyList(list(low_speed_limit = low_speed_limit,
                                      low_speed_time = low_speed_time),
                                 options)
    h <- curl::handle_setopt(h, .list = options)
    return(h)
  }

  database <- list()

  on.exit({
    if (length(database) != 0) {
      jsonlite::write_json(database, database_file, pretty = TRUE)
    }
  })

  map_pool <- curl::new_pool(total_con = total_connections,
                             host_con = host_connections)



  retry_maybe <- function(i, message = "") {
    unlink(files[i])   # Clean up unfinished files.
    attempts[i] <<- attempts[i] + 1

    if (attempts[i] <= retry) {
      message <- paste0(message, tr_(".. retrying"))
      # It would be great to add support for resuming the download.
      if (file.exists(files[i])) {
        pb$tick(len = -file.size(files[i]))  # remove downloaded data from progress
        pb$message(msg = message)
      }

      add_job(i)
    }
    delay_fn()
  }

  done_fn <- function(resp, i) {
    resp$content <- NULL  # Remove content to free up memory.
    out[[i]] <<- resp

    ## Sometimes request are completed with bad status
    if (resp$status_code > 200) {
      message <-tr_("Failed to download: %s", basename(files[i]))

      retry_maybe(i, message)
      return(NULL)
    }

    ## Check local hash. This might be a bit slow, but it's important.
    local_hash <- digest::digest(file = files[i],
                                 algo = tolower(metadata[[i]]$checksum_type[[1]]))

    if (local_hash != metadata[[i]]$checksum[[1]]) {
      message <- tr_("Downloaded file hash doesn't match.")

      retry_maybe(i, message)
      return(NULL)
    }

    message <- tr_("Successfuly downloaded: %s", basename(files[i]))


    ## Write this file in the database
    database <<- append(database, metadata[i])

    if (!pb$finished) {
      pb$message(msg = message)
    }
    delay_fn()
  }


  fail_fn <- function(err, i) {
    out[[i]] <- err
    retry_maybe(i)

    if (!pb$finished) {
      pb$message(msg = tr_("Failed: %s", basename(files[i])))
      pb$message(msg = tr_("with error: %s", err))
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




format_bytes <- function (x) {
  powers <- c(k = 1, M = 2, G = 3, T = 4, P = 5, E = 6, Z = 7, Y = 8)

  symbols <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")

  base <- 1000

  power <- findInterval(abs(x), base^powers)
  symbol <- symbols[1L + power]

  paste0(signif(x/base^power, 3), " ", symbol)
}
