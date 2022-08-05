

tr_ <- function(...) {
  gettextf(..., domain = "R-rcmip6")
}


cmip6_folder_template <- "%(root)s/%(mip_era)s/%(activity_drs)s/%(institution_id)s/%(source_id)s/%(experiment_id)s/%(member_id)s/%(table_id)s/%(variable_id)s/%(grid_label)s/%(version)s"

cmip6_file_template <- c("%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s_%(time_start)s-%(time_end)s.nc",
                         "%(variable_id)s_%(table_id)s_%(source_id)s_%(experiment_id)s_%(member_id)s_%(grid_label)s.nc")

# from https://pcmdi.llnl.gov/mips/cmip5/docs/cmip5_data_reference_syntax_v1-02_marked.pdf
cmip5_folder_template <- "%(root)s/%(project)s/%(product)s/%(institute)s/%(model)s/%(experiment)s/%(time_frequency)s/%(realm)s/%(cmor_table)s/%(ensemble)s/%(version)s/%(variable)s/"
