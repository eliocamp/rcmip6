#' List downloaded models.
#'
#' Reads files in your local CMIP6 directory and parses the contents into a
#' data.frame
#'
#' @inheritParams cmip_download
#'
#' @returns
#' A data.frame.
#'
#' @export
cmip_available <- function(root = cmip_root_get()) {
  infos <- list.files(root, pattern = "model.info", recursive = TRUE, full.names = TRUE)

  cmip6_folder_template <- gsub("%\\(", "", cmip6_folder_template)
  cmip6_folder_template <- gsub("\\)s", "", cmip6_folder_template)

  vars <- c(strsplit(cmip6_folder_template, "/")[[1]],
            "variable_long_name",
            "datetime_start",
            "datetime_stop",
            "nominal_resolution")
  vars <- setdiff(vars, "root")

  data <- Reduce(rbind,
         lapply(infos, function(info) {
           files <- list.files(dirname(info), pattern = ".nc", recursive = TRUE, full.names = TRUE)
           data <- jsonlite::read_json(info, simplifyVector = TRUE)

           info <- as.data.frame(data[vars])

           info$files <- list(files)
           info$full_info <- list(data)
           info
         }))

  return(data)
}
