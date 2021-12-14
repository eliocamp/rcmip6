#' List downloaded models.
#'
#' @inheritParams cmip_download
#' @export
cmip_available <- function(base_dir = cmip_folder_get()) {
  info <- list.files(base_dir, pattern = "model.info", recursive = TRUE, full.names = TRUE)

  cmip6_folder_template <- gsub("%\\(", "", cmip6_folder_template)
  cmip6_folder_template <- gsub("\\)s", "", cmip6_folder_template)

  vars <- c(strsplit(cmip6_folder_template, "/")[[1]],
            "variable_long_name",
            "datetime_start",
            "datetime_stop",
            "nominal_resolution")

  data <- Reduce(rbind,
         lapply(info, function(info) {
           data <- jsonlite::read_json(info, simplifyVector = TRUE)
           data <- as.data.frame(data[vars])

           files <- list.files(dirname(info), pattern = ".nc", recursive = TRUE, full.names = TRUE)
           data$files <- list(files)
           data
         }))

  return(data)
}
