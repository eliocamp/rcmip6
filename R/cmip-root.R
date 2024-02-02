#' Set or get the root folder for CMIP data.
#'
#' @param root Root folder.
#' @param mode Mode that defines folder permissions. Can be an umask valid
#' string or the aliases:
#'  - "default": the default umask returned by `Sys.umask(NA)`.
#'  - "shared": read and write permissions to any user (mode "0000")
#'  - "private": read and write permissions only for the current user
#'  (mode "7777")
#'
#'@export
cmip_root_set <- function(root, mode = "default") {
  options(RCMIP6_ROOT = path.expand(root))

  mode <- switch(mode,
                 shared = "0000",
                 private = "7",
                 default = Sys.umask(NA),
                 mode)
  options(RCMIP6_ROOT_MODE = mode)

}

#' @export
#' @rdname cmip_root_set
cmip_root_get <- function() {
  path.expand(getOption("RCMIP6_ROOT",
            stop(tr_("Root folder not specified. Use cmip_root_set()."))))
}


