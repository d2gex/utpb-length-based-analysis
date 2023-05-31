library("data.table")
library("lubridate")
library("testit")
library("assertr")
library("tidyverse")
library("openxlsx")
library("quanteda")



# (0) Read different stylesheets

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

hauls_with_different_length <- c()
for (id_lance in id_lances_sample) {

  old_test <- old_cut_df %>%
    select(Idlance) %>%
    filter(Idlance == id_lance)
  new_test <- new_df %>%
    select(Idlance) %>%
    filter(Idlance == id_lance)
  if (nrow(old_test) != nrow(new_test)) {
    hauls_with_different_length <- append(hauls_with_different_length, id_lance)
  }
}
testit::assert("There are hauls with different number of rows", length(hauls_with_different_length) > 0)

# (7) Get hauls for a serious of columns exactly the same
id_lances_sample <- setdiff(id_lances_sample, hauls_with_different_length)
testit::assert("Number of hauls left is total - hauls with different rows",
               length(id_lances_sample_bk) == length(id_lances_sample) + length(hauls_with_different_length))

sought_columns <- c('Idlance', 'ESPECIE', 'PUERTO_EMBARQUE', 'Madurez',
                    'NUMINDIVS', 'N TRIPUS', 'ARTE', 'Piezas', 'ZONA', 'OBSER1')

old_test <- old_cut_df %>%
  select_at(.vars = sought_columns) %>%
  filter((Idlance %in% id_lances_sample)) %>%
  arrange_at(.vars = sought_columns)
new_test <- new_df %>%
  select_at(.vars = sought_columns) %>%
  filter((Idlance %in% id_lances_sample)) %>%
  arrange_at(.vars = sought_columns)

testit::assert("Both dataframes have the same length after removing hauls with different rows",
               nrow(old_test) == nrow(new_test))
testit::assert("Old and new dataframe have the same especies", all.equal(old_test, new_test))

# (8) Find Idlances for group of columns that have different values

potential_diff_columns <- list(
  'TALLA' = c('Idlance', 'TALLA'),
  'PESO' = c('Idlance', 'PESO'),
  'OVADA' = c('Idlance', 'OVADA'),
  'Colorhuevos' = c('Idlance', 'Colorhuevos'),
  'CoD' = c('Idlance', 'CoD'),
  'HorafV' = c('Idlance', 'HorafV'),
  'HorafL' = c('Idlance', 'HorafL'),
  'FVIR' = c('Idlance', 'FVIR'),
  'FLARG' = c('Idlance', 'FLARG'),
  'mcarte1' = c('Idlance', 'mcarte1')
)
columns_and_diff_idlances <- get_diff_between_columns(id_lances_sample, potential_diff_columns, old_cut_df, new_df)
columns_and_diff_idlances[['Piezas']] <- absence_id_lances_new_df
if (length(hauls_with_different_length) > 0) {
  columns_and_diff_idlances[['ESPECIE']] <- hauls_with_different_length
}
excel_sheets_list <- build_sheet_list_of_different_cols(columns_and_diff_idlances, old_cut_df, new_df)
write.xlsx(excel_sheets_list, file = "../data/sensitive/output/old_new_db_differences.xlsx")
