source("config.R")
source("data_filter.R")
library("tidyverse")

read_db_details <- function(path_to_csv) {
  if (!DB_READ) {
    utpb_original_db <- read.csv(path_to_csv)
    DB_READ <- TRUE
  }

  return (utpb_original_db)
}

run <- function() {
  db_data <- read_db_details(DB_TALLAS_PATH)
  # db_filer <- DbDataFilter$new(db_data)
  # db_filer$rename_columns(c)
}

run()