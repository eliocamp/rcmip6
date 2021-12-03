# Code for the old package.
if (FALSE) {
#' Lista datos de CMIP6 disponibles en una carpeta
#'
#' @param base_dir carpeta raiz de la base de datos
#' @export
#' @importFrom stats na.omit
cmip_available <- function(base_dir = cmip_folder_get()) {

  # if (!file.exists(file.path(base_dir, ".cima_cmip6"))) {
  # stop("No CMIP6 folder structure in ", base_dir, ". Mount it first wiht `cmip_mount(base_dir = ", base_dir, ").")
  # }


  # base_dir <- "~/DATOS/CMIP6/"
  files <- list.files(base_dir, recursive = TRUE)
  download <- vapply(files, function(f) strsplit(f, "/")[[1]][1] == "Download", TRUE)
  files <- files[!download]
  pattern <- .cmip_pattern(type = "ensemble", ext = "nc4")
  # pattern <- "{experiment_id}/{frequency}/{variable_id}/{variable_id}_  Amon_{source_id}_{experiment_id}_{datetime_start}-{datetime_stop}.nc4"

  available <- unglue::unglue_data(files, pattern, multiple = unique)

  details <- .parse_member_id(available$member_id)
  available <- cbind(available, details)


  available$file <- file.path(base_dir, files)
  # available <- stats::na.omit(available)

  if (requireNamespace("ncdf4", quietly = TRUE)) {
    info <- do.call(rbind, lapply(seq_len(nrow(available)), function(i) {
      nc <-  ncdf4::nc_open(available[i, ][["file"]])
      ens_len <- ncdf4::nc_open(available[i, ][["file"]])$dim$ensemble$len
      lon_res <- 360 / nc$dim$lon$len
      lat_res <- 180 / nc$dim$lat$len

      levs <- nc$dim$plev$len
      if (is.null(levs)) levs <- 1

      data.frame(n_members = ens_len,
                 lon_res = lon_res,
                 lat_res = lat_res,
                 n_levs = levs)
    }))

    available <- cbind(available, info)
  } else {
    message("ncdf4 library not installed. Install with `install.packages(\"ncdf4\") to get more information")
  }

  available$size <- file.info(available$file)$size

  return(available)
}











.cmip_pattern <- function(type = c("ensamble", "member"), ext = "{ext}") {
  pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_{table_id}_{source_id}_{experiment_id}_{member_id}_{grid_label}_{datetime_start}-{datetime_stop}.", ext)
  # if (type[1] == "member") {
  #   pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_{table_id}_{source_id}_{experiment_id}_{variant_label}_{grid_label}_{datetime_start}-{datetime_stop}.", ext)
  # } else {
  #   pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_{table_id}_{source_id}_{experiment_id}_{sub_experiment_id}_i{initialization_index}p{physics_index}f{forcing_index}_{grid_label}_{datetime_start}-{datetime_stop}.", ext)
  # }

  return(pattern)
}

.cmip_parse_filename <- function(filename) {

  pattern <- .cmip_pattern()



}


.cmip_cima_experiments <- function(experiment_id) {
  map <- c("historical"   = "Historical",
           "hist-GHG"     = "HistoricalGHG",
           "hist-nat"     = "HistoricalNat",
           "hist-stratO3" = "HistoricalO3",
           "hist-sol"     = "HistoricalSol")

  out <- map[experiment_id]
  if (is.na(out)) {
    out <- experiment_id
  }

  return(unname(out))
}






#' Concatena los archivos NetCDF de distintos miembros del mismo experimento
#'
#' @param files vector con los archivos a consolidar (por ejemplo, la dalida de [cmip_download()]).
#' Si es `NULL`, consolida todos los archivos en `base_dir`.
#' @param base_dir carpeta raiz de la base de datos. Sólo usado si `files` es `NULL`.
#'
#'
#' @return
#' Un vector de caracteres con los archivos consolidados.
#'
#' @export
cmip_consolidate <- function(files = NULL, base_dir) {
  old_mode <- Sys.umask(getOption("CIMADATA.MODE", default = Sys.umask(NA)))
  on.exit(
    Sys.umask(old_mode)
  )

  if (is.null(files)) {
    download_dir <- paste0(base_dir, "/Download/Format/Data_used")
    files <- list.files(download_dir, recursive = TRUE, pattern = ".nc", full.names = TRUE)
  }
  files <- files[!is.na(files)]

  data <- unglue::unglue_data(files, paste0("{base_dir}/Download/Format/Data_used/",
                                            .cmip_pattern("member", ext = "nc")),
                              multiple = unique)
  data <- cbind(data, .parse_member_id(data$member_id))
  data$file <- files
  data$remove <- FALSE

  uniques <- with(data, interaction(experiment_id, sub_experiment_id, frequency, variable_id, source_id,
                                    initialization_index, physics_index, forcing_index,
                                    grid_label, drop = TRUE))
  out <- split(data, uniques)
  unlist(lapply(out, function(dt) {
    on.exit({
      #Remove temporary files, if present
      tempfiles <- list.files(unique(dt$base_dir), pattern = "*.ncecat.tmp",
                              recursive = TRUE, full.names = TRUE)
      file.remove(tempfiles)

      # Remove other files
      file.remove(dt[dt$remove == TRUE, ]$file)
    }, add = TRUE)

    if (nrow(dt) == 0) {
      return(NULL)
    }

    dt_future <- dt
    dt_future$datetime_start <- min(dt_future$datetime_start)
    dt_future$datetime_stop <- max(dt_future$datetime_stop)
    out_file <- dt_future[1, ]
    out_file[["member_id"]] <- gsub("r\\d+", "", out_file[["member_id"]])
    out_file <- glue::glue_data(out_file, paste0("{base_dir}/", .cmip_pattern("ensemble", ext = "nc4")))[1]

    if (file.exists(out_file)) {
      message_time(out_file, " already exists. Skipping.")
      return(out_file)
    }

    message_time(paste0(c("Processing files:", dt$file), collapse = "\n"))

    # Join dates
    members <- split(dt, dt$realization_index)
    members <- lapply(members, function(m) {
      if (nrow(m) == 1) {
        return(m)
      }
      in_files <- paste0(m$file, collapse = " ")

      m$datetime_start <- min(m$datetime_start)
      m$datetime_stop <- max(m$datetime_stop)
      m <- m[1, ]
      out_file <- glue::glue_data(m, paste0("{base_dir}/", .cmip_pattern("member", ext = "nc")))[1]

      m$file <- out_file
      m$remove <- TRUE

      if (file.exists(out_file)) {
        message_time("Member #", unique(m$realization_index), " already concatenated. Skipping.")
        return(m)
      }

      dest_dir <- dirname(out_file)
      if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
      }


      command <- glue::glue("ncrcat --ovr {in_files} {out_file}")
      message_time("Concatenating member #", unique(m$realization_index), " with command:\n", command)
      system(command)
      return(m)
    })

    dt <- do.call(rbind, members)

    dest_dir <- dirname(out_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    in_files <- paste0(dt$file, collapse = " ")
    command <- glue::glue("ncecat --ovr -M -4 -u ensemble {in_files} {out_file}")
    message_time("Merging members into ", out_file, " with command: \n", command)
    system(command)


    out_file
  }))
}



message_time <- function(..., domain = NULL, appendLF = TRUE) {
  text <- paste0(...)
  time <- paste0("[", Sys.time(), "]")
  message(paste0(time, ": ", text), domain = domain, appendLF = appendLF)
}


}
