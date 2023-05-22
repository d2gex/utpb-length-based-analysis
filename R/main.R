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

  db_filter <- DbDataFilter$new(db_data)

  # (1) Rename georeference columns
  col_name_mapping <- list(
    'LON.inicio' = 'start_long',
    'LAT.inicio' = 'start_lat',
    'LON.final' = 'end_long',
    'LAT.final' = 'end_lat'
  )
  old_columns <- names(col_name_mapping)
  new_columns <- unname(col_name_mapping)
  db_filter$rename_columns(old_columns, unlist(new_columns))
  # db_filter$clean_df %>% verify(do.call(has_all_names, new_columns))

  # (2) Convert all potential datetime columns as such
  datetime_columns <- c('dia', 'FLARG', 'FVIR', 'HorafL', 'HorafV')
  db_filter$to_datetime(datetime_columns)

  # (3) Extract NaN values for dia, largada and virada columns
  # db_filter$split_df_by_blank_fields(datetime_columns)

  return(db_filter$clean_df)
}

db_data <- read_db_details(DB_TALLAS_PATH)
clean_db_data <- run(db_data)
