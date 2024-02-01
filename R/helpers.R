

tr_ <- function(...) {
  gettextf(..., domain = "R-rcmip6")
}


year_range_overlaps <- function(titles, year_range) {
  if (any(!is.finite(year_range))) {
    return(rep(TRUE, length(titles)))
  }

  # The date is in a format of 6 digits separated by a dash or underscore
  # Capture the first four digits of each group of digits.
  date_range_regex <- "(\\d{4})\\d{2}[-_](\\d{4})\\d{2}"
  dates <- utils::strcapture(date_range_regex, titles,
                             proto = list(file_date_start = integer(),
                                          file_date_end = integer()))
  file_date_start <- dates$file_date_start
  file_date_end <- dates$file_date_end

  # Get the intersections of windows specified by user and dates contained in the file
  # These statements could be nested, but are not too expensive anyway
  # Is the file fully inside the window?
  file_inside_window <- (year_range[1] <= file_date_start) & (file_date_end <= year_range[2])
  # Is the window specified by the user within the file?
  window_inside_file <- (file_date_start <= year_range[1]) & (file_date_end >= year_range[2])
  # Does the window intersect the start of the file?
  left <- (year_range[1] <= file_date_start) & (file_date_start <= year_range[2])
  # Does the window intersect the end of the file?
  right <- (year_range[1] <= file_date_end) & (file_date_end <= year_range[2])
  # Does the window partially contain the file?
  file_touches_window <- left | right

  window_in_file <- file_touches_window | file_inside_window | window_inside_file

  return(window_in_file)

}



cmip6_folder_template <- "%(root)s/%(mip_era)s/%(activity_drs)s/%(institution_id)s/%(source_id)s/%(experiment_id)s/%(member_id)s/%(table_id)s/%(variable_id)s/%(grid_label)s/%(version)s"

cmip6_file_template <- c("%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s_%(time_start)s-%(time_end)s.nc",
                         "%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s.nc")

# from https://pcmdi.llnl.gov/mips/cmip5/docs/cmip5_data_reference_syntax_v1-02_marked.pdf
cmip5_folder_template <- "%(root)s/%(project)s/%(product)s/%(institute)s/%(model)s/%(experiment)s/%(time_frequency)s/%(realm)s/%(cmor_table)s/%(ensemble)s/%(version)s/%(variable)s/"
