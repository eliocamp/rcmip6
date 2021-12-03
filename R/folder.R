#' Set or get the root folder for CMIP data.
#'
#' @param base_dir Root folder.
#' @param mode Mode that defines fodler permissions. Can be an umask valid string or
#' the aliases:
#'  - "default": the default umask returned by `Sys.umask(NA)`.
#'  - "shared": read and write permissions to any user (mode "0000")
#'  - "private": read and write permissions only for the current user (mode "7777")
#'
#'@export
cmip_folder_set <- function(base_dir, mode = "default") {
  options(RCMIP6_BASEDIR = path.expand(base_dir))

  mode <- switch(mode,
                 shared = "0000",
                 private = "7",
                 default = Sys.umask(NA),
                 mode)
  options(RCMIP6_BASEDIR_MODE = mode)

}

#' @export
#' @rdname cmip_folder_set
cmip_folder_get <- function() {
  path.expand(getOption("RCMIP6_BASEDIR",
            stop(tr_("Root folder not specified. Use cmip_folder_set()."))))
}


