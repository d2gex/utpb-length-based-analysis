library("R6")
library("data.table")
library("sf")

BaseDataFilter <- R6Class("BaseDataFilter", public = list(
  db_data = NULL,
  clean_df = NULL,
  dirty_df = data.frame(),
  initialize = function(clean_data, dirty_data) {
    self$db_data <- clean_data
    self$clean_df <- copy(clean_data)
    if (!missing(dirty_data)) {
      self$dirty_df <- copy(dirty_data)
    }
  },
  add_to_dirty = function(df, error_description) {
    # Add error description to dirty_df
    df <- df %>%
      mutate(error = error_description)
    if (nrow(self$dirty_df) == 0) {
      self$dirty_df <- copy(df)
    }
    else {
      new_columns <- setdiff(names(df), names(self$dirty_df))
      self$dirty_df[new_columns] <- 'artifitcially created'
      self$dirty_df <- rbind(self$dirty_df, df)
    }
  },
  add_to_clean = function(df) {
    self$clean_df <- rbind(self$clean_df, df)
  },
  set_clean_data = function(df) {
    self$clean_df <- copy(df)
  },
  set_dirty_data = function(df) {
    self$dirty_df <- copy(df)
  },
  get_quick_map_data = function(fields) {
    self$clean_df %>%
      select_at(.vars = fields) %>%
      unique()
  }
))

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

get_column_data_type <- function(df, column_name) {
  return(
    sapply(df, class)[column_name]
  )
}

make_cols_same_length <- function(col_1, col_2, fill) {

  if (length(col_1) == length(col_2)) {
    return(list(col_1 = col_1, col_2 = col_2))
  }

  if (length(col_1) > length(col_2)) {
    col_min <- col_2
    col_max <- col_1
    col_1_max <- TRUE
  }
  else if (length(col_2) > length(col_1)) {
    col_min <- col_1
    col_max <- col_2
    col_1_max <- FALSE
  }
  num_rows_diff <- length(col_max) - length(col_min)
  col_min <- c(col_min, rep(fill, num_rows_diff))
  if (col_1_max) {
    return(list(col_1 = col_max, col_2 = col_min))
  }
  return(list(col_1 = col_min, col_2 = col_max))
}

get_name <- function(var_name) {
  return(deparse(substitute(var_name)))
}

from_crs_to_crs <- function(df, lon, lat, crs_source, crs_dest) {

  coords <- c(lon, lat)
  df <- df %>%
    select_at(.vars = coords) %>%
    st_as_sf(coords = coords) %>%
    st_set_crs(crs_source) %>%
    st_transform(crs_dest) %>%
    mutate(!!lon := sf::st_coordinates(.)[, 2],
           !!lat := sf::st_coordinates(.)[, 1])

  result <- data.frame(
    matrix(ncol = 2,
           nrow = nrow(df),
           dimnames = list(NULL, c(lon, lat))
    ))
  result[lon] <- df[lon]
  result[lat] <- df[lat]
  return(result)

}

replace_columns <- function(df_to, df_from, replaced_columns, condition_columns) {
          #' Replaces the a subset of columns in df_to from df_from, assuming that the columns to be
          #' replaced have the name name in both dataframes and that only the rows from df_to should
          #' be returned

  # (1) Get subset of relevant columns
  relevant_columns <- c(replaced_columns, condition_columns)
  from_df <- df_from %>% select_at(.vars = relevant_columns)

  # (2) Rename columns in the data from which we will get the replacements
  from_df <- from_df %>%
    rename_with(.fn = ~paste0(., "_from"), .cols = replaced_columns)

  # (3) Add replaced columns to df_to and get rid of the ones of itself
  df_to <- merge(df_to, from_df, by.x = condition_columns, all.x = TRUE) %>%
    select(-replaced_columns)

  # (4) Restore replaced columns with original names
  replaced_columns_from <- paste(replaced_columns, "_from", sep = "")
  df_to <- data.table::setnames(df_to, old = replaced_columns_from, new = replaced_columns)
  return(df_to)
}
