library("data.table")
library("lubridate")
library("testit")
library("assertr")
library("tidyverse")

old_db_capturas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_CAPTURAS_16-05-2018.csv",
                               locale = locale(encoding = 'latin1'))

if (!exists('old_db_capturas'))
  old_db_capturas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_CAPTURAS_16-05-2018.csv",
                               locale = locale(encoding = 'latin1'))
if (!exists('new_db_capturas'))
  new_db_capturas <- read_csv2("../data/sensitive/CONSULTA BDP_UTPB_CAPTURAS_12-04-2023.CSV",
                               locale = locale(encoding = 'latin1'))
if (!exists('old_db_tallas'))
  old_db_tallas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_TALLAS_16-05-2018.csv",
                             locale = locale(encoding = 'latin1'))

if (!exists('new_db_tallas'))
  new_db_tallas <- read_csv2("../data/sensitive/CONSULTA BDP_UTPB_TALLAS_17-04-2023_.csv",
                             locale = locale(encoding = 'latin1'))

fLat <- function(x) {
  trunc(x / 100000) + ((x - (100000 * trunc(x / 100000))) / 1000) / 60
}

fLon <- function(x) {
  -(trunc(x / 100000) + ((x - (
    100000 * trunc(x / 100000)
  )) / 1000) / 60)
}

prepare_df <- function(df) {
  df <- df %>%
    mutate(
      lon_ini_dec = fLon(start_long),
      lon_fin_dec = fLon(end_long),
      lat_ini_dec = fLat(start_lat),
      lat_fin_dec = fLat(end_lat),
    ) %>%
    mutate(
      lon_dec = case_when(
        is.na(lon_ini_dec) ~ lon_fin_dec,
        .default = lon_fin_dec
      ),
      lat_dec = case_when(
        is.na(lat_ini_dec) ~ lat_fin_dec,
        .default = lat_ini_dec
      )
    ) %>%
    mutate(
      lon_or = case_when(
        is.na(start_long) ~ end_long,
        .default = start_long
      ),
      lat_or = case_when(
        is.na(start_lat) ~ end_lat,
        .default = start_lat
      )
    ) %>%
    mutate(
      HorafL = as.POSIXct(HorafL, format = "%d/%m/%Y %H:%M", tz = "UTC"),
      HorafV = as.POSIXct(HorafV, format = "%d/%m/%Y %H:%M", tz = "UTC"),
      FLARG = as.POSIXct(FLARG, format = "%d/%m/%Y %H:%M", tz = "UTC"),
      FVIR = as.POSIXct(FVIR, format = "%d/%m/%Y %H:%M", tz = "UTC")
    )
  return(df)
}

build_coords_graph <- function(df) {
  g <- df %>%
    ggplot(aes(x = lon_dec, y = lat_dec, col = ZONA)) +
    geom_point() +
    coord_fixed(1.3)
  return(g)
}

# (1) Rename georeference columns
col_name_mapping <- list(
  'LON inicio' = 'start_long',
  'LAT inicio' = 'start_lat',
  'LON final' = 'end_long',
  'LAT final' = 'end_lat'
)
old_cols <- names(col_name_mapping)
new_cols <- unname(col_name_mapping)

data.table::setnames(new_db_capturas, old = old_cols, new = unlist(new_cols))
data.table::setnames(old_db_capturas, old = old_cols, new = unlist(new_cols))
data.table::setnames(new_db_tallas, old = old_cols, new = unlist(new_cols))
data.table::setnames(old_db_tallas, old = old_cols, new = unlist(new_cols))

# (2) Prepare dataframes to get the longitude and latitude and strings as datetime objects
old_db_capturas <- prepare_df(old_db_capturas)
new_db_capturas <- prepare_df(new_db_capturas)
old_db_tallas <- prepare_df(old_db_tallas)
new_db_tallas <- prepare_df(new_db_tallas)

# (3) Get the oldest date appearing in the old dataframe

max_oldb <- max(old_db_tallas$HorafV, old_db_tallas$FVIR, old_db_tallas$FLARG, old_db_tallas$HorafL, na.rm = T)
max_newdb <- max(new_db_tallas$HorafV, new_db_tallas$FVIR, new_db_tallas$FLARG, new_db_tallas$HorafL, na.rm = T)
mute <- old_db_tallas %>%
  filter(is.na(HorafV) | HorafV <= max_oldb) %>%
  verify(nrow(.) == nrow(old_db_tallas))


# (4) Get a snapshot of relevant columns up to the maximum date in the old dataframe, which means
# the entire old dataframe and the new one

old_df <- old_db_tallas %>%
  filter(is.na(HorafV) | HorafV <= max_oldb) %>%
  rename("lon_dec_old" = "lon_dec", "lat_dec_old" = "lat_dec", "lon_or_old" = "lon_or", "lat_or_old" = "lat_or") %>%
  select(Idlance, lon_dec_old, lat_dec_old, lon_or_old, lat_or_old, ESPECIE, CoD,
         NUMINDIVS, PESO, SEXO, OVADA, TALLA, Madurez) %>%
  arrange(Idlance)

new_df <- new_db_tallas %>%
  filter(is.na(HorafV) | HorafV <= max_oldb) %>%
  rename("lon_dec_new" = "lon_dec", "lat_dec_new" = "lat_dec", "lon_or_new" = "lon_or", "lat_or_new" = "lat_or") %>%
  select(Idlance, lon_dec_new, lat_dec_new, lon_or_new, lat_or_new, ESPECIE, CoD,
         NUMINDIVS, PESO, SEXO, OVADA, TALLA, Madurez) %>%
  arrange(Idlance)


# (5) Remove from the old dataframe the idlances missing in the new dataframe and compare length
absence_id_lances_new_df <- setdiff(old_df$Idlance, new_df$Idlance)
old_cut_df <- old_df %>%
  filter(!Idlance %in% absence_id_lances_new_df)

mute <- old_cut_df %>%
  filter(Idlance %in% absence_id_lances_new_df) %>%
  verify(nrow(.) == 0)

testit::assert("After removing idlances from old_df, both dataframes don't yet have the same length",
               nrow(old_df) != nrow(new_df))


# Remove from the old tallas dataframe the excess rows that appear in some hauls
old_cut_df <- old_cut_df %>% rowid_to_column('ID')
new_df <- new_df %>% rowid_to_column('ID')
excess_rows_per_idlances <- setdiff(old_cut_df$ID, new_df$ID)
excess_rows_in_old_db <- old_cut_df %>% filter(ID %in% excess_rows_per_idlances)
old_cut_df <- old_cut_df %>% filter(!(ID %in% excess_rows_per_idlances))

mute <- old_cut_df %>%
  filter(ID %in% excess_rows_per_idlances) %>%
  verify(nrow(.) == 0)
mute <- old_cut_df %>% verify(nrow(.) == nrow(new_df))