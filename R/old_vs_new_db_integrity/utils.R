library("data.table")
source("utils.R")

fLat <- function(x) {
  trunc(x / 100000) + ((x - (100000 * trunc(x / 100000))) / 1000) / 60
}

fLon <- function(x) {
  -(trunc(x / 100000) + ((x - (
    100000 * trunc(x / 100000)
  )) / 1000) / 60)
}

prepare_geo_and_time_cols <- function(df) {
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

build_coords_graph <- function(df, title) {
  g <- df %>%
    ggplot(aes(x = lon_dec, y = lat_dec, col = ZONA)) +
    geom_point() +
    coord_fixed(1.3) +
    ggtitle(title)
  return(g)
}

get_lances_for_columns_with_different_values <- function(id_lances, column_pairs, df_1, df_2) {

  col_with_diff_id_lances <- list()
  for (col_name in names(column_pairs)) {
    print(paste("------>Processing column '", col_name, "' ..."))
    diff_id_lances <- c()
    for (id_lance in id_lances) {
      tested_columns <- column_pairs[[col_name]]
      df_1_test <- df_1 %>%
        select_at(.vars = tested_columns) %>%
        filter(Idlance == id_lance)

      df_2_test <- df_2 %>%
        select_at(.vars = tested_columns) %>%
        filter(Idlance == id_lance)
      if (!isTRUE(all.equal(df_1_test, df_2_test))) {
        diff_id_lances <- append(diff_id_lances, id_lance)
      }
    }
    col_with_diff_id_lances[[col_name]] <- diff_id_lances
    print(paste("------> End"))
  }
  return(col_with_diff_id_lances)
}

build_list_of_column_with_associated_data <- function(lances_per_column, df_1, df_2) {

  sheets_list <- list()
  for (col_name in names(lances_per_column)) {

    # (1) Get all rows that which IdLance matches a list of lances
    subset_df_1 <- df_1 %>%
      filter(Idlance %in% lances_per_column[[col_name]]) %>%
      select(Idlance, !!col_name)
    subset_df_2 <- df_2 %>%
      filter(Idlance %in% lances_per_column[[col_name]]) %>%
      select(Idlance, !!col_name)

    id_lances <- subset_df_1$Idlance
    col_old <- subset_df_1[[col_name]]
    col_new <- subset_df_2[[col_name]]

    # (2) Ensure columns of old and new dataframe match in length by padding in the columns with less elements
    if (length(col_old) != length(col_new)) {
      col_type <- get_column_data_type(subset_df_1, col_name)
      if (col_type == 'numeric') {
        fill <- -Inf
      }
      else {
        fill <- '-Inf'
      }
      equal_col_lengths <- make_cols_same_length(col_1 = col_old, col_2 = col_new, fill = fill)
      col_old <- equal_col_lengths[['col_1']]
      col_new <- equal_col_lengths[['col_2']]
      if (nrow(subset_df_2) > nrow(subset_df_1)) {
        id_lances <- subset_df_2$Idlance
      }
    }

    # (3) Create the dataframe and add it to the sheet list
    df <- data.frame(matrix(ncol = 3, nrow = length(col_old), dimnames = list(NULL, c('Idlance', col_name, paste0(col_name, '_new')))))
    df$Idlance <- id_lances
    df[[col_name]] <- col_old
    df[[paste0(col_name, '_new')]] <- col_new
    sheets_list[[col_name]] <- df
  }
  return(sheets_list)

}