#' Downloads CMIP data
#'
#' @param results una lista de resultados devuelta por [cmip_search()]
#' @param base_dir carpeta raiz de la base de datos de CIP6
#' @param user usuario para autenticar (ver [cmip_key_set])
#' @param system_config comandos de sistema para correr antes de iniciar las descargas
#' (por ejemplo, para setear el proxy).
#' @param force_https usar hptts? (ver Detalles)()
#'
#' @param progress_bar Mostrar una barra de progreso?
#'
#' @details
#' Los archivos se descargan en una estructura de carpetas estándard:
#' \[base_dir\]/Download/Format/Data_used/\{experiment_id\}/\{frequency\}/\{variable_id\}/\{variable_id\}_\{table_id\}_\{source_id\}_\{experiment_id\}_r\{realization_index\}i\{initialization_index\}p\{physics_index\}f\{forcing_index\}_\{grid_label\}_\{datetime_start\}-\{datetime_stop\}.\{ext\}
#'
#' Algunos modelos fallan al descargarse con el script original porque éste usa http y el servidor
#' espera https. Con `force_https = TRUE` se cambia la URL interna del script para usar https.
#'
#' @return
#' Un vector de caracteres con los archivos descargados (que puede pasarse directamente a [cmip_consolidate()])
#'
#' @export
cmip_download <- function(results, base_dir = cmip_folder_get()) {
  base_dir <- path.expand(base_dir)
  cmip_download_one(result, base_dir = base_dir)

}

cmip_download_one <- function(result, base_dir = cmip_folder_get()) {
  dir <- result_dir(result, root = base_dir)
  dir.create(dir, FALSE, TRUE)

  message(glue::glue(tr_("Downloading {result$title}...")))
  url <- paste0("https://", result$index_node, "/search_files/", result$id, "/", result$index_node, "/?limit=999")

  info <- httr::content(httr::GET(url))$response$docs

  i <- info[[1]]
  i$url
  urls <- vapply(info, function(x) x[["url"]][[1]], character(1))

  urls <- vapply(strsplit(urls, "\\|"), "[", character(1), 1)


  files <-   vapply(info, function(i) {

    url <- strsplit(i$url[[1]], "\\|")[[1]][1]
    file <- file.path(dir, i$title)

    if (file.exists(file)) {
      checksum_type  <- tolower(i$checksum_type[[1]])
      checksum <- i$checksum[[1]]
      local_checksum <- digest::digest(file = file, algo = checksum_type)

      if (local_checksum == checksum) {
        message(tr_("Skipping existing file with good checksum."))
        return(file)
      }
    }
    download.file(url = url,
                  destfile = file)
    file
  }, character(1))
}



result_dir <- function(result, root = cmip_folder_get()) {
  glue::glue_data(result,
                  result[["directory_format_template_"]][[1]],
                  .open = "%(",
                  .close = ")s"
  )
}

result_filename <- function(result) {
  glue::glue_data(result,
                  result[["dataset_id_template_"]][[1]],
                  .open = "%(",
                  .close = ")s"
  )
}



#' Computes the total size of a search result in Mb.
#'
#' @inheritParams cmip_download
#' @export
cmip_size <- function(results) {
  res <- sum(vapply(results, function(r) r$size, FUN.VALUE = 1))/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}


#' @export
print.cmip_size <- function(x, ...) {
  cat(signif(x, 3), "Mb")
}




