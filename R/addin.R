# nocov start
url_to_list_Addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection[[1]]$text
  request <- cmip_url_to_list(selection)

  request <- list_pretty_format(request)

  location <- context$selection[[1]]$range
  rstudioapi::modifyRange(location = location, text = request, id = context$id)
}

# nocov end
