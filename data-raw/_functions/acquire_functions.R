
# ABOUT -------------------------------------------------------------------

# Functions to facilitate acquiring data

# Direct downloads --------------------------------------------------------

get_zip_data <- function(url, target_dir, force = FALSE) {
  # Function: to download and decompress a .zip file to a target directory
  
  # Check to see if the data already exists
  if(!dir.exists(target_dir) | force == TRUE) { # if data does not exist, download/ decompress
    cat("Creating target data directory \n") # print status message
    dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create target data directory
    cat("Downloading data... \n") # print status message
    temp <- tempfile() # create a temporary space for the file to be written to
    download.file(url = url, destfile = temp) # download the data to the temp file
    unzip(zipfile = temp, exdir = target_dir, junkpaths = TRUE) # decompress the temp file in the target directory
    cat("Data downloaded! \n") # print status message
  } else { # if data exists, don't download it again
    cat("Data already exists \n") # print status message
  }
}


get_compressed_data <- function(url, target_dir, force = FALSE) {
  # Get the extension of the target file
  ext <- tools::file_ext(url)
  # Check to see if the target file is a compressed file
  if(!ext %in% c("zip", "gz", "tar")) stop("Target file given is not supported")
  # Check to see if the data already exists
  if(!dir.exists(target_dir) | force == TRUE) { # if data does not exist, download/ decompress
    cat("Creating target data directory \n") # print status message
    dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create target data directory
    cat("Downloading data... \n") # print status message
    temp <- tempfile() # create a temporary space for the file to be written to
    download.file(url = url, destfile = temp) # download the data to the temp file
    # Decompress the temp file in the target directory
    if(ext == "zip") {
      unzip(zipfile = temp, exdir = target_dir, junkpaths = TRUE) # zip files
    } else {
      untar(tarfile = temp, exdir = target_dir) # tar, gz files
    }
    cat("Data downloaded! \n") # print status message
  } else { # if data exists, don't download it again
    cat("Data already exists \n") # print status message
  }
}

