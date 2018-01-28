
# ABOUT -------------------------------------------------------------------

# Description: Acquire and curate the Brown Corpus
# Usage: Internet connection required
# Author: Jerid Francom
# Date: 2018-01-28

# SETUP -------------------------------------------------------------------

pacman::p_load(tidyverse)

# _ Functions -------------------------------------------------------------

source("data-raw/_functions/acquire_functions.R")

parse_xml_doc <- function(file) {
  # Function: reads a Brown XML file and returns a tidy dataset
  # with the attributes `document_id`, `category`, `words`, and `pos`

  require(xml2)

  cat("Reading", basename(file), "... ") # status update

  doc <- read_xml(file) # read xml document
  doc %>% xml_ns_strip() # remove the namespace to simplify path matching

  document_id <-
    doc %>%
    xml_find_all("//text") %>% # isolate element
    xml_attr("id") %>% # extract attribute
    str_extract("\\d+") # extract digits

  category <-
    doc %>%
    xml_find_all("//text") %>% # isolate element
    xml_attr("decls") # extract attribute

  words <-
    doc %>%
    xml_find_all("//w|//c") %>% # isolate elements
    xml_text() # extract text

  pos <-
    doc %>%
    xml_find_all("//w|//c") %>% # isolate elements
    xml_attr("type") # extract attribute

  data <-
    data_frame(document_id, category, words, pos) # create tidy dataset

  cat("Done.\n") # status update

  return(data) # make the function output the dataset
}

# RUN ---------------------------------------------------------------------

# Download Brown Corpus (BROWN) -------------------------------------------

# Download tei version (xml) http://www.nltk.org/nltk_data/
get_compressed_data(url = "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/brown_tei.zip", target_dir = "data-raw/original/brown/")

# Tidy Brown language data ------------------------------------------------

# Get the paths to the corpus files
files <-
  list.files(path = "data-raw/original/brown/", # directory where the files are
             pattern = "^\\w\\d+", # only return files starting with letter + digits
             full.names = TRUE) # return the full path

# Read files and return a tidy dataset
brown_data <-
  files %>% # pass files
  map(parse_xml_doc) %>% # iteratively process each file
  bind_rows() # bind the resulting data frames into one

# Tidy Brown meta-data ----------------------------------------------------

# Get category description information from `Corpus.xml` file
doc <- read_xml(x = "data-raw/original/brown/Corpus.xml") # read xml

category <-
  doc %>%
  xml_find_all("//d1:category") %>% # isolate elements
  xml_attr("id") # extract attributes

category_description <-
  doc %>%
  xml_find_all("//d1:category") %>% # isolate elements
  xml_text(trim = TRUE) # extract text (trim any whitespace)

brown_categories <-
  data_frame(category, category_description) # create a data frame

# Join brown_data and brown_categories
brown <-
  left_join(brown_data, brown_categories) %>% # join by `category`
  select(document_id, category, category_description, words, pos) # arrange column order

# Write data to disk ------------------------------------------------------

# Write the curated dataset to the `data-raw/derived/` directory
write_csv(x = brown, path = "data-raw/derived/brown.csv")

# Write the curated dataset to the `data/` directory
save(brown, file = "data/brown.rdata")

# CLEANUP -----------------------------------------------------------------

rm(list = ls()) # clean up objects
