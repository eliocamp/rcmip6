
.check_system <- function(package) {
  out <- suppressWarnings(system(glue::glue("which {package}"), intern = TRUE))

  if (length(out) == 0) {
    stop("Pacakge ", package," not installed.")
  }
}


.pattern_python_to_r <- function(x) {
  x <- gsub("%(", "{", x, fixed = TRUE)
  x <- gsub(")s", "}", x, fixed = TRUE)
  x
}

tr_ <- function(...) {
  gettextf(..., domain = "R-rcmip6")
}

