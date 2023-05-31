library("R6")
library("data.table")

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
    return(
      self$clean_df %>% select_at(.vars = fields)
    )
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

build_coords_graph <- function(df, title) {
  g <- df %>%
    ggplot(aes(x = lon_dec, y = lat_dec, col = ZONA)) +
    geom_point() +
    coord_fixed(1.3) +
    ggtitle(title)
  return(g)
}

get_diff_between_columns <- function(id_lances, column_pairs, df_1, df_2) {

  col_with_diff_id_lances <- list()
  for (name in names(column_pairs)) {
    print(paste("------>Processing column '", name, "' ..."))
    diff_id_lances <- c()
    for (id_lance in id_lances) {
      tested_columns <- column_pairs[[name]]
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
    col_with_diff_id_lances[[name]] <- diff_id_lances
    print(paste("------> End"))
  }
  return(col_with_diff_id_lances)
}

build_sheet_list_of_different_cols <- function(diff_id_lances, df_1, df_2) {

  sheets_list <- list()
  for (name in names(diff_id_lances)) {

    # (1) Get all rows that which IdLance matches a list of lances
    subset_df_1 <- df_1 %>%
      filter(Idlance %in% diff_id_lances[[name]]) %>%
      select(Idlance, !!name)
    subset_df_2 <- df_2 %>%
      filter(Idlance %in% diff_id_lances[[name]]) %>%
      select(Idlance, !!name)

    col_old <- subset_df_1[[name]]
    col_new <- subset_df_2[[name]]

    # (2) Ensure columns of old and new dataframe match in length by padding in the columns with less elements
    if (length(col_old) != length(col_new)) {
      col_type <- get_column_data_type(subset_df_1, name)
      if (col_type == 'numeric') {
        fill <- -Inf
      }
      else {
        fill <- '-Inf'
      }
      equal_col_lengths <- make_cols_same_length(col_1 = col_old, col_2 = col_new, fill = fill)
      col_old <- equal_col_lengths[[col_1]]
      col_new <- equal_col_lengths[[col_2]]
    }

    # (3) Create the dataframe and add it to the sheet list
    df <- data.frame()
    df[name] <- col_old
    df[paste0(name, '_new')] <- col_new
    sheets_list[[name]] <- df
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
