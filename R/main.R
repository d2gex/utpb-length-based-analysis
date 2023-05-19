source("config.R")
source("data_filter.R")
library("tidyverse")
library("assertr")

read_db_details <- function(path_to_csv) {
  if (!DB_READ) {
    utpb_original_db <- read.csv(path_to_csv)
    DB_READ <- TRUE
  }
  return(utpb_original_db)
}

run <- function(db_data) {

  db_filer <- DbDataFilter$new(db_data)
  old_columns <- names(COL_NAME_MAPPING)
  new_columns <- unname(COL_NAME_MAPPING)
  db_filer$rename_columns(old_columns, unlist(new_columns))
  db_filer$db_data %>% verify(do.call(has_all_names, new_columns))
  return(db_filer$db_data)
}

db_data <- read_db_details(DB_TALLAS_PATH)
clean_db_data <- run(db_data)
