library("data.table")
library("lubridate")
library("testit")
library("assertr")
library("tidyverse")
library("openxlsx")
library("quanteda")
library("R6")
source("utils.R")

IntegrityData <- R6Class("IntegrityData", public = list(

  col_name_mapping = list(
    'LON inicio' = 'start_long',
    'LAT inicio' = 'start_lat',
    'LON final' = 'end_long',
    'LAT final' = 'end_lat'
  ),
  excluded_columns = c("lon_ini_dec", "lon_fin_dec", "lat_ini_dec", "lat_fin_dec",
                       "lon_dec", "lat_dec", "lon_or", "lat_or"),
  sought_columns = c(),
  potential_diff_columns = list()
))

DbVersionComparator <- R6Class("DbVersionComparator", public = list(
  old_db_data = NULL,
  new_db_data = NULL,
  integrity_data = NULL,
  max_sample_number = NULL,
  initialize = function(old_db_data, new_db_data, integrity_data, max_sample_number) {
    self$old_db_data <- old_db_data
    self$new_db_data <- new_db_data
    self$integrity_data <- integrity_data
    self$max_sample_number <- max_sample_number
  },
  rename_columns = function() {
    # (1) Rename georeference columns
    old_cols <- names(self$integrity_data$col_name_mapping)
    new_cols <- unname(self$integrity_data$col_name_mapping)
    data.table::setnames(self$old_db_data, old = old_cols, new = unlist(new_cols))
    data.table::setnames(self$new_db_data, old = old_cols, new = unlist(new_cols))
  },
  prepare_dataframes = function() {
    # (2) Prepare dataframes to get the longitude and latitude and strings as datetime objects
    self$old_db_data <- prepare_geo_and_time_cols(self$old_db_data)
    self$new_db_data <- prepare_geo_and_time_cols(self$new_db_data)
  },
  get_max_dates_from_old_db = function() {
    # (3) Get the oldest date appearing in the old dataframe
    max_oldb <- max(self$old_db_data$HorafV, self$old_db_data$FVIR, self$old_db_data$FLARG, self$old_db_data$HorafL, na.rm = T)
    mute <- self$old_db_data %>%
      filter(is.na(HorafV) | HorafV <= max_oldb) %>%
      verify(nrow(.) == nrow(self$old_db_data))
    return(max_oldb)
  },
  fetch_data_from_dataframes_up_to_date = function(max_oldb) {
    # (4) Get a snapshot of relevant columns up to the maximum date in the old dataframe, which means
    # the entire old dataframe and the new one

    old_df <- self$old_db_data %>%
      filter(is.na(HorafV) | HorafV <= max_oldb) %>%
      select(-self$integrity_data$excluded_columns) %>%
      arrange(Idlance, ESPECIE, TALLA)

    new_df <- self$new_db_data %>%
      filter(is.na(HorafV) | HorafV <= max_oldb) %>%
      select(-self$integrity_data$excluded_columns) %>%
      arrange(Idlance, ESPECIE, TALLA)
    return(list(old_df = old_df, new_df = new_df))
  },
  detract_absent_id_laces_in_new_db = function(old_df, new_df) {
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

    return(list(absent_lances = absence_id_lances_new_df, old_cut_df = old_cut_df))
  },
  get_random_lances_sample = function(old_cut_df) {
    return(sort(sample(unique(old_cut_df$Idlance), self$max_sample_number)))
  },
  fetch_id_lances_with_different_num_rows = function(id_lances_sample, old_cut_df, new_df) {
    # (6) Get hauls with different number of rows

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
    return(hauls_with_different_length)
  },
  check_lances_with_same_numrows = function(id_lances_sample, hauls_with_different_length, old_cut_df, new_df) {
    # (7) Get hauls for a serious of columns exactly the same
    id_lances_same_numrows <- setdiff(id_lances_sample, hauls_with_different_length)
    testit::assert("Number of hauls left is total - hauls with different rows",
                   length(id_lances_sample) == length(id_lances_same_numrows) + length(hauls_with_different_length))
    old_test <- old_cut_df %>%
      select_at(.vars = self$integrity_data$sought_columns) %>%
      filter((Idlance %in% id_lances_same_numrows)) %>%
      arrange_at(.vars = self$integrity_data$sought_columns)
    new_test <- new_df %>%
      select_at(.vars = self$integrity_data$sought_columns) %>%
      filter((Idlance %in% id_lances_same_numrows)) %>%
      arrange_at(.vars = self$integrity_data$sought_columns)

    testit::assert("Both dataframes have the same length after removing hauls with different rows",
                   nrow(old_test) == nrow(new_test))
    testit::assert("Old and new dataframe have the same especies", all.equal(old_test, new_test))
  },
  run = function() {

    self$rename_columns()
    self$prepare_dataframes()
    max_oldb <- self$get_max_dates_from_old_db()
    results <- self$fetch_data_from_dataframes_up_to_date(max_oldb)
    old_df <- results$old_df
    new_df <- results$new_df
    results <- self$detract_absent_id_laces_in_new_db(old_df, new_df)
    absent_lances <- results$absent_lances
    old_cut_df <- results$old_cut_df
    id_lances_sample <- self$get_random_lances_sample(old_cut_df)
    lances_with_diff_num_rows <- self$fetch_id_lances_with_different_num_rows(id_lances_sample, old_cut_df, new_df)
    self$check_lances_with_same_numrows(id_lances_sample, lances_with_diff_num_rows, old_cut_df, new_df)
    return(list(
      id_lances_sample = id_lances_sample,
      absent_lances = absent_lances,
      lances_with_diff_num_rows = lances_with_diff_num_rows,
      old_df = old_df,
      old_cut_df = old_cut_df,
      new_df = new_df
    ))
  }

))

# run_db_comparator <- function(old_db_data,
#                               new_db_data,
#                               integrity_data,
#                               max_sample) {

output_path <- "../data/sensitive/output/old_new_db_differences.xlsx"
# if (!exists('old_db_capturas'))
#   old_db_capturas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_CAPTURAS_16-05-2018.csv",
#                                locale = locale(encoding = 'latin1'))
# if (!exists('new_db_capturas'))
#   new_db_capturas <- read_csv2("../data/sensitive/CONSULTA BDP_UTPB_CAPTURAS_12-04-2023.CSV",
#                                locale = locale(encoding = 'latin1'))
if (!exists('old_db_tallas'))
  old_db_tallas <- read_csv2("../data/sensitive/consulta_utpb_2018/CONSULTA BDP_UTPB_TALLAS_16-05-2018.csv",
                             locale = locale(encoding = 'latin1'))

if (!exists('new_db_tallas'))
  new_db_tallas <- read_csv2("../data/sensitive/CONSULTA BDP_UTPB_TALLAS_17-04-2023_.csv",
                             locale = locale(encoding = 'latin1'))
integrity_data <- IntegrityData$new()
integrity_data$sought_columns <- c('Idlance', 'ESPECIE', 'PUERTO_EMBARQUE', 'Madurez',
                                   'NUMINDIVS', 'N TRIPUS', 'ARTE', 'Piezas', 'ZONA', 'OBSER1')
integrity_data$potential_diff_columns <- list(
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
db_comparator <- DbVersionComparator$new(old_db_tallas, new_db_tallas, integrity_data, 1000)
result <- db_comparator$run()
# Get data that need to be reported because integrity issues

# ---> Get lances associated to columns which have same length
# but different values in the old vs new dataframes
lances_per_column <- get_lances_for_columns_with_different_values(result$id_lances_sample,
                                                                  integrity_data$potential_diff_columns,
                                                                  result$old_cut_df,
                                                                  result$new_df)
# --->  Add lances that have different number of rows for the old and new dataframe
if (length(result$hauls_with_different_length) > 0) {
  lances_per_column[['ESPECIE']] <- result$hauls_with_different_length
}
columns_with_associated_data <- build_list_of_column_with_associated_data(lances_per_column,
                                                                          result$old_cut_df,
                                                                          result$new_df)


# -->  Now get the actual data for lances that are absent in the new dataframe
lances_per_column <- list(Piezas = result$absent_lances)
added_columns_data <- build_list_of_column_with_associated_data(lances_per_column,
                                                                old_db_tallas %>% select(Idlance, Piezas),
                                                                result$new_df)
columns_with_associated_data[['Piezas']] <- added_columns_data[['Piezas']]
# return(columns_with_associated_data)
# }

# run <- function() {
# (0) Read different stylesheets


# excel_data <- run_db_comparator(old_db_tallas, new_db_tallas, integrity_data, 1000)
write.xlsx(columns_with_associated_data, file = output_path)


# }
