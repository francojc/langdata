
# ABOUT -------------------------------------------------------------------

# Description: Acquire and curate the Switchboard Dialog Act Corpus
# Usage: Internet connection required
# Author: Jerid Francom
# Date: 2018-01-28

# SETUP -------------------------------------------------------------------

pacman::p_load(tidyverse)

# _ Functions -------------------------------------------------------------

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

extract_swda_metadata <- function(file) {
  # Function: to read a Switchboard Corpus Dialogue file and extract metadata
  cat("Reading", basename(file), "...")
  doc <- read_lines(file) # read file by lines

  # Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
  doc_speaker_info <-
    doc[str_detect(doc, "\\d+_\\d+_\\d+")] %>% # isolate pattern
    str_extract("\\d+_\\d+_\\d+") %>% # extract the pattern
    str_split(pattern = "_") %>% # split the character vector
    unlist() # flatten the list to a character vector
  doc_id <- doc_speaker_info[1] # extract `doc_id`
  speaker_a_id <- doc_speaker_info[2] # extract `speaker_a_id`
  speaker_b_id <- doc_speaker_info[3] # extract `speaker_b_id`

  # Extract `text`
  text_start_index <- # find where header info stops
    doc %>%
    str_detect(pattern = "={3,}") %>% # match 3 or more `=`
    which() # find vector index

  text_start_index <- text_start_index + 1 # increment index by 1
  text_end_index <- length(doc)

  text <- doc[text_start_index:text_end_index] # extract text
  text <- str_trim(text) # remove leading and trailing whitespace
  text <- text[text != ""] # remove blank lines

  data <- data.frame(doc_id, text) # tidy format `doc_id` and `text`

  data <- # extract column information from `text`
    data %>%
    mutate(damsl_tag = str_extract(string = text, pattern = "^.+?\\s")) %>%  # extract damsl tags
    mutate(speaker_turn = str_extract(string = text, pattern = "[AB]\\.\\d+")) %>% # extract speaker_turn pairs
    mutate(utterance_num = str_extract(string = text, pattern = "utt\\d+")) %>% # extract utterance number
    mutate(utterance_text = str_extract(string = text, pattern = ":.+$")) %>%  # extract utterance text
    select(-text)

  data <-
    data %>%
    separate(col = speaker_turn, into = c("speaker", "turn_num")) # separate speaker_turn into distinct columns

  data <- # clean up column information
    data %>%
    mutate(damsl_tag = str_trim(damsl_tag)) %>% # remove leading/ trailing whitespace
    mutate(utterance_num = str_replace(string = utterance_num, pattern = "utt", replacement = "")) %>% # remove 'utt'
    mutate(utterance_text = str_replace(string = utterance_text, pattern = ":\\s", replacement = "")) %>% # remove ': '
    mutate(utterance_text = str_trim(utterance_text)) # trim leading/ trailing whitespace

  data <- # link speaker with speaker_id
    data %>%
    mutate(speaker_id = case_when(
      speaker == "A" ~ speaker_a_id,
      speaker == "B" ~ speaker_b_id
    ))
  cat(" done.\n")
  return(data) # return the data frame object
}

# RUN ---------------------------------------------------------------------

# Download SWitchboard Dialog Act Corpus (SWDA) ---------------------------

# Resource information: https://catalog.ldc.upenn.edu/LDC97S62
# Download corpus annotations
get_compressed_data(url = "https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz", target_dir = "data-raw/original/swda/")

# Tidy SWDA language data -------------------------------------------------

# Get the paths to the corpus files
files <-
  list.files(path = "data-raw/original/swda",
             pattern = "\\.utt",
             full.names = TRUE,
             recursive = TRUE)

# Read files and return a tidy dataset
swda <-
  files %>% # pass file names
  map(extract_swda_metadata) %>% # read and tidy iteratively
  bind_rows() # bind the results into a single data frame


# Tidy SWDA meta-data -----------------------------------------------------

swda_speaker_meta <-
  read_csv(file = "https://catalog.ldc.upenn.edu/docs/LDC97S62/caller_tab.csv",
           col_names = c("speaker_id", # changed from `caller_no`
                         "pin",
                         "target",
                         "sex",
                         "birth_year",
                         "dialect_area",
                         "education",
                         "ti",
                         "payment_type",
                         "amt_pd",
                         "con",
                         "remarks",
                         "calls_deleted",
                         "speaker_partition"))

swda_speaker_meta <- # remove double quotes
  swda_speaker_meta %>%
  map(str_replace_all, pattern = '"', replacement = '') %>% # iteratively replace doubled quotes
  bind_rows() %>%  # combine the results by rows
  type_convert() # return columns to orignal data types

glimpse(swda_speaker_meta) # preview the dataset

swda_speaker_meta <- # select columns of interest
  swda_speaker_meta %>%
  select(speaker_id, sex, birth_year, dialect_area, education)

# Join `sdac` with `sdac_speaker_meta` by `speaker_id`
swda$speaker_id <- swda$speaker_id %>% as.numeric() # convert to integer
swda <- left_join(swda, swda_speaker_meta) # join by `speaker_id`

glimpse(swda) # preview the joined dataset

# Diagnostics
swda[!complete.cases(swda), ] %>% glimpse # view incomplete cases
swda[!complete.cases(swda), ] %>% select(speaker_id) %>% unique() # id speaker(s) with incomplete information

swda <- # remove speaker 155
  swda %>%
  filter(speaker_id != 155)

# Write the curated dataset to the `data-raw/derived/` directory
write_csv(x = swda, path = "data-raw/derived/swda.csv")

# Write the curated dataset to the `data/` directory
save(swda, file = "data/swda.Rdata")

# CLEANUP -----------------------------------------------------------------

rm(list = ls()) # clean up objects
