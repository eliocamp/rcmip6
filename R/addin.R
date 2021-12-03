

url_to_list_Addin <- function() {
  context   <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection[[1]]$text
  request  <- cmip_url_to_list(selection)

  max_n <- max(vapply(names(request), nchar, 0))


  elements <- lapply(seq_along(request), function(i) {
    paste0("  ",
           formatC(names(request)[i], width = -max_n, flag = " "),
           " = \"",
           request[[i]], "\"")
  })

  request <- paste0("query <- list(\n",
                paste0(unlist(elements), collapse = ",\n"),
                "\n)\n")

  location <- context$selection[[1]]$range
  rstudioapi::modifyRange(location = location,
                          text = request,
                          id = context$id)
}

