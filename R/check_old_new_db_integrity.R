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
excluded_columns <- c("lon_ini_dec", "lon_fin_dec", "lat_ini_dec", "lat_fin_dec", "lon_dec", "lat_dec", "lon_or", "lat_or")

old_df <- old_db_tallas %>%
  filter(is.na(HorafV) | HorafV <= max_oldb) %>%
  # rename("lon_dec_old" = "lon_dec", "lat_dec_old" = "lat_dec", "lon_or_old" = "lon_or", "lat_or_old" = "lat_or") %>%
  select(-excluded_columns) %>%
  arrange(Idlance, ESPECIE, TALLA)

new_df <- new_db_tallas %>%
  filter(is.na(HorafV) | HorafV <= max_oldb) %>%
  # rename("lon_dec_new" = "lon_dec", "lat_dec_new" = "lat_dec", "lon_or_new" = "lon_or", "lat_or_new" = "lat_or") %>%
  select(-excluded_columns) %>%
  arrange(Idlance, ESPECIE, TALLA)

# (5) Remove from the old dataframe the missing idlances in the new dataframe and compare length
absence_id_lances_new_df <- setdiff(old_df$Idlance, new_df$Idlance)
old_cut_df <- old_df %>%
  filter(!Idlance %in% absence_id_lances_new_df)

mute <- old_cut_df %>%
  filter(Idlance %in% absence_id_lances_new_df) %>%
  verify(nrow(.) == 0)

testit::assert("Old and new dataframes STILL have different number of rows after removing differetn Idlances",
               nrow(old_cut_df) != nrow(new_df))

testit::assert("Old and new dataframes have the same IdLance identifier",
               sum(unique(old_cut_df$Idlance)) == sum(unique(new_df$Idlance)))

# (6) Get hauls with different number of rows
id_lances_sample <- sort(sample(unique(old_cut_df$Idlance), 1000))
id_lances_sample_bk <- copy(id_lances_sample)

diff_length <- c()
for (id_lance in id_lances_sample) {

  old_test <- old_cut_df %>%
    select(Idlance) %>%
    filter(Idlance == id_lance)
  new_test <- new_df %>%
    select(Idlance) %>%
    filter(Idlance == id_lance)
  if (nrow(old_test) != nrow(new_test)) {
    diff_length <- append(diff_length, id_lance)
  }
}
testit::assert("There are hauls with different number of rows", length(diff_length) > 0)

# (7) Get hauls for that a serious of columns exactly the same
id_lances_sample <- setdiff(id_lances_sample, diff_length)
testit::assert("Number of hauls left is total - hauls with different rows",
               length(id_lances_sample_bk) == length(id_lances_sample) + length(diff_length))

string_or_integer_cols <- c('Idlance', 'ESPECIE', 'PUERTO_EMBARQUE',  'Madurez',
                            'NUMINDIVS',  'N TRIPUS', 'ARTE', 'Piezas', 'ZONA', 'OBSER1')

old_test <- old_cut_df %>%
  select_at(.vars = string_or_integer_cols) %>%
  filter((Idlance %in% id_lances_sample)) %>%
  arrange_at(.vars = string_or_integer_cols)
new_test <- new_df %>%
  select_at(.vars = string_or_integer_cols) %>%
  filter((Idlance %in% id_lances_sample)) %>%
  arrange_at(.vars = string_or_integer_cols)

testit::assert("Both dataframes have the same length after removing hauls with different rows",
               nrow(old_test) == nrow(new_test))
testit::assert("Old and new dataframe have the same especies", all.equal(old_test, new_test))

# (8)




# write.csv(head(old_cut_df, 1000), "../data/sensitive/output/old_df.csv")
# write.csv(head(new_df, 1000), "../data/sensitive/output/new_df.csv")









