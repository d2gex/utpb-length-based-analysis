source("config.R")
source("data_filter.R")
library("tidyverse")
library("assertr")

db_data <- read.csv(DB_TALLAS_PATH)
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

# (3) Get rid of rows that either HorafL and FLARG are both NaN or HorafV and FVIR are both NaN
largarda_columns <- c('FLARG', 'HorafL')
db_filter$get_rid_of_NaNs_for_all_cols(largarda_columns)
mute <- db_filter$clean_df %>%
  filter(if_all(largarda_columns, ~is.na(.))) %>% # Not NaNs in both columns at the same time
  verify(nrow(.) == 0)

virada_columns <- c('FVIR', 'HorafV')
db_filter$get_rid_of_NaNs_for_all_cols(virada_columns)
mute <- db_filter$clean_df %>%
  verify(nrow(.) + nrow(db_filter$dirty_df) == nrow(db_filter$db_data)) %>%  # dirty + clean = all
  filter(if_all(largarda_columns, ~is.na(.))) %>% # Not NaNs in both columns at the same time
  verify(nrow(.) == 0)

# (4) Ensure that largada and virada times are integral: virada and largada are not NA and largada <= virada
db_filter$extract_largada_virada_dates()
mute <- db_filter$clean_df %>%
  verify(not_na(largada_time)) %>%
  verify(not_na(virada_time)) %>%
  verify(largada_time <= virada_time) %>%
  verify(soak_time >= 0) %>%
  verify(!do.call(has_all_names, list('aux_largada', 'aux_virada')))

# (5) Convert string-based columns into ASCII.

# Need to covert this specific case not to lose the value once the encoding and transform occurs
db_filter$clean_df <- db_filter$clean_df %>%
  mutate(across(.cols = PUERTO_EMBARQUE,
                ~ifelse(str_detect(., "Cibrao_Porti"), "San Cribao_Porto de Morás", .))) %>%
  mutate(across(.cols = PUERTO_EMBARQUE,
                ~ifelse(str_detect(., "Louriz"), "Lourizán (Pontevedra)", .)))

fields <- c('ZONA', 'PUERTO_EMBARQUE', 'ARTE', 'ESPECIE', 'valor')
db_filter$to_encoding(fields, encoding = 'ASCII', string_transform = "Latin-ASCII")

all_ascii <- function(x) return(unique(stri_enc_mark(x)) == 'ASCII')
mute <-
  db_filter$clean_df %>%
    assert(function(x) return(not_na(x)),
           ZONA, PUERTO_EMBARQUE, ARTE, ESPECIE, valor) %>%
    assert(function(x) return(unique(stri_enc_mark(x)) == 'ASCII'),
           ZONA, PUERTO_EMBARQUE, ARTE, ESPECIE,valor)
