source("config.R")
source("data_filter.R")
library("tidyverse")
library("testit")
library("assertr")

read_db_details <- function(path_to_csv) {
  if (!DB_READ) {
    utpb_original_db <- read.csv(path_to_csv)
    DB_READ <- TRUE
  }
  return(utpb_original_db)
}


db_data <- read_db_details(DB_TALLAS_PATH)
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
mute <- db_filter$clean_df %>% verify(do.call(has_all_names, new_columns))

# (2) Convert all potential datetime columns as such
datetime_columns <- c('dia', 'FLARG', 'FVIR', 'HorafL', 'HorafV')
db_filter$to_datetime(datetime_columns)

# (3) Get rid of rows that either HorafL or FLARG are NaN
largarda_columns <- c('FLARG', 'HorafL')
db_filter$get_rid_of_NaNs(largarda_columns)
mute <- db_filter$dirty_df %>%
  verify(is.na(FLARG)) %>%
  verify(is.na(HorafL))
testit::assert("dirty + clean != data",
               nrow(db_filter$db_data) == nrow(db_filter$clean_df) + nrow(db_filter$dirty_df))

