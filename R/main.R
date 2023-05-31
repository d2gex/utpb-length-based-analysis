source("config.R")
source("common_filter.R")
source("coords_filter.R")
library("tidyverse")
library("assertr")
library("data.table")

if (!exists('db_data')) {
  db_data <- read_csv2(DB_TALLAS_PATH, locale = locale(encoding = 'latin1'))
}
db_filter <- DbDataFilter$new(db_data)

# (1) Rename georeference columns
col_name_mapping <- list(
  'LON inicio' = 'start_long',
  'LAT inicio' = 'start_lat',
  'LON final' = 'end_long',
  'LAT final' = 'end_lat'
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
db_filter$adhoc_replacements()
fields <- c('ZONA', 'PUERTO_EMBARQUE', 'ARTE', 'ESPECIE', 'valor')
db_filter$to_encoding(fields, encoding = 'ASCII', string_transform = "Latin-ASCII")

all_ascii <- function(x) return(unique(stri_enc_mark(x)) == 'ASCII')
mute <-
  db_filter$clean_df %>%
    assert(function(x) return(not_na(x)),
           ZONA, PUERTO_EMBARQUE, ARTE, ESPECIE, valor) %>%
    assert(function(x) return(unique(stri_enc_mark(x)) == 'ASCII'),
           ZONA, PUERTO_EMBARQUE, ARTE, ESPECIE, valor)

# (6) Classify seafloor type
hard_seafloor_options <- list('hard' = c("piedra", "roca y algas", "piedra dura"))
mixed_seafloor_options <- list(
  'mixed' = c("cascos de barcos y/o bateas",
              "piedra y arena",
              "algas, roca y arena",
              "piedra y fango",
              "fango-pedra-cascallo",
              "piedra y coral",
              "pedra-cascallo",
              "arena-coral-piedra",
              "cascajo con arena y piedras")
)
default_substrata <- 'soft'
unknown_substrata <- 'unknown'
substrata_type <- c(names(hard_seafloor_options),
                    names(mixed_seafloor_options),
                    default_substrata,
                    unknown_substrata)

db_filter$classify_seafloor(hard_seafloor_options,
                            mixed_seafloor_options,
                            default_substrata,
                            unknown_substrata)

mute <- db_filter$clean_df %>%
  assert(function(x) return(x %in% substrata_type), seafloor)

# To avoid having to rerun the whole script due to the code below modifies the
# the clean_df dataframe irreversibly
clean_filter_df <- copy(db_filter$clean_df)
dirty_filter_df <- copy(db_filter$dirty_df)

# (7) Get rows which longitude and latitude pairs have got at least one value

long_lat_filter <- LongLatFilter$new(db_filter$clean_df, db_filter$dirty_df)
long_fields <- c('start_long', 'end_long')
lat_fields <- c('start_lat', 'end_lat')
long_lat_filter$get_rid_of_NaNs(long_fields, lat_fields)

# --> Ensure that in the clean data there is no rows with all geo-fields as NA
mute <- long_lat_filter$clean_df %>%
  filter(if_all(c(long_fields, lat_fields), ~is.na(.))) %>%
  verify(nrow(.) == 0)

# --> Ensure that there if the LONGITUDE pair has got at least ONE value its LATITUDE counterpart does too
mute <- long_lat_filter$clean_df %>%
  filter(if_any(long_fields, ~not_na(.))) %>%
  filter(if_all(lat_fields, ~is.na(.))) %>%
  verify(nrow(.) == 0)

# --> Ensure that there if the LATITUDE pair has got at least ONE value its LONGITUDE counterpart does too
mute <- long_lat_filter$clean_df %>%
  filter(if_any(lat_fields, ~not_na(.))) %>%
  filter(if_all(long_fields, ~is.na(.))) %>%
  verify(nrow(.) == 0)

# (8) Transform longitud and latitude to decimal degree
long_lat_filter$to_espg_4326()

# --> Make sure that every pair of geopoint has a value
mute <- long_lat_filter$clean_df %>%
  filter(if_any(c('lon', 'lat'), ~is.na(.))) %>%
  verify(nrow(.) == 0)

# # (9) Transform coordinates to UTM 29
crs_esp_4326 <- "+init=epsg:4326"
crs_esp_25829 <- "+init=epsg:25829"
long_lat_filter$from_crs_to_crs(crs_esp_4326, crs_esp_25829)

lat_clean_df <- copy(long_lat_filter$clean_df)
lat_dirty_df <- copy(long_lat_filter$dirty_df)

mute <- long_lat_filter$clean_df %>%
  filter(if_any(c('lon_utm', 'lat_utm'), ~is.na(.))) %>%
  verify(nrow(.) == 0)


# # (x) Concatenate clean and dirty dataframes
clean_df <- copy(long_lat_filter$clean_df)
dirty_df <- (long_lat_filter$dirty_df)
