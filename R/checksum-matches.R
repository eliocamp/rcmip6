checksum_matches <- function(file, checksum_type, checksum) {
    checksum_file <- paste0(file, ".chksum")

  if (!file.exists(file)) {
    return(FALSE)
  }

  if (file.exists(checksum_file)) {
    local_checksum <- readLines(checksum_file)
  } else {
    local_checksum <- digest::digest(file = file, algo = tolower(checksum_type))
    writeLines(text = local_checksum, con = checksum_file)
  }

  return(local_checksum == checksum)
}


