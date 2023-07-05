library("data.table")
library("sf")
library("testit")
library("assertr")
library("dplyr")
library("readr")

read_csv_input <- function(filename) {
  return(as.data.frame(
    read_csv(filename, locale = locale(encoding = 'latin1'))
  ))
}

get_column_data_type <- function(df, column_name) {
  return(
    sapply(df, class)[column_name]
  )
}

create_empty_dataframe <- function(col_names) {
  df <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
  colnames(df) <- col_names
  return(df)
}

create_empty_plot <- function(empty_message, title) {
  g <- ggplot() +
    theme_void() +
    geom_text(aes(0, 0, label = empty_message)) +
    xlab(NULL) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  return(g)
}

get_nearest_base <- function(x) {
  return(10^ceiling(log10(x)))
}

is_single_string <- function(input) {
  is.character(input) & length(input) == 1
}

build_yearly_date_intervals <- function (day, month, date_sep, start_year, end_year, year_step) {
  years <- seq(start_year, end_year, year_step)
  date_intervals <- list()
  for (x in 2:length(years)) {
    start <- ifelse(x==2, start_year, years[x-1])
    previous_date <- paste0(day, date_sep, month, date_sep, start)
    next_date <- paste0(day, date_sep, month, date_sep, years[x])
    date_intervals[[x-1]] <- c(previous_date, next_date)
  }
  return(date_intervals)
}

generate_catch_at_length_freq_table <- function(data, bindwith, variable, reference) {
  # // @formatter:off
  #' Generate a long dataframe containing the number of individuals per year and size interval (and its midpoint). The
  #' size of each interval is given by 'bindwidth' and all values of 'variable' in the datatrame are taken into account
  #' @param bindwidth: size of each length interval in cms
  #' @param variable: column of the dataframe for which intervals are generated (i.e., talla, peso, etc..)
  #' @param reference: column of the dataframe used as reference for which intervals need to be replicated (i.e.:,
  #' typically a timescale such as year)
  # // @formatter:on

  # (1) Create intervals and midpoints over the whole time series
  unique_refs <- unique(data[[reference]])
  min <- floor(min(data[, variable]))
  max <- ceiling(max(data[, variable]))
  half_bindwith <- bindwith / 2
  unique_intervals <- seq(min, max, bindwith)
  mid_points <- seq(min + half_bindwith, max - half_bindwith, bindwith)
  lengt_test <- length(unique_intervals) == length(mid_points) + 1
  testit::assert(deparse(lengt_test), lengt_test)

  # (2) Build the individual yearly catch at length frequency table
  # and concat them all together
  columns <- c('year', 'interval', 'midpoint', 'freq')
  catch_length_df <- create_empty_dataframe(columns)
  for (ref in unique_refs) {
    #  --> Build frequency table
    year_data <- data %>% filter_at(.vars = reference, ~.x == ref)
    year_intervals <- as.data.frame(
      table(
        cut(year_data[, variable], unique_intervals),
        dnn = columns[2]),
      responseName = columns[4])
    # --> Add midpoints and year columns
    year_intervals <- year_intervals %>%
      mutate(
        !!columns[3] := mid_points,
        !!reference := ref
      ) %>%
      select_at(.vars = columns)
    # --> concat year intervals together
    catch_length_df <- rbind(catch_length_df, year_intervals)

  }
  return(catch_length_df)
}

make_cols_same_length <- function(col_1, col_2, fill) {
  # // @formatter:off
  #' Ensure to given columns have the same length by filling up the shotest columns with a give
  #' filling value
  # // @formatter:on

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
  # @formatter:off
  #' Return a dataframe with two coordinate-columns converted to a given CRS. The name of the columns
  #' is the same as in the original dataframe df
  # @formatter:on

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
  # @formatter:off
  #' Replaces a subset of columns in df_to from df_from, assuming that the columns being
  #' replaced have the same name in both dataframes and that all rows from df_to should
  #' be returned
  # @formatter:on

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

add_text_to_graph_position <- function(gg_plot, df, x_col, y_col, label_col, label_text_size, vertical_adjustment_function) {
  # @formatter:off
  #' Add the value of one column in the passed dataframe on top of each bar. The vertical adjustment
  #' is calculated by the passed function
  # @formatter:on
  for (row in 1:nrow(df)) {
    x <- df[row, x_col]
    y <- ifelse(is_single_string(y_col), # is y_col the name of the column or an array of values where to place the text?
                vertical_adjustment_function(df[row, y_col]),
                vertical_adjustment_function(y_col[row]))
    label <- df[row, label_col]
    gg_plot <- gg_plot + annotate("text", x = x, y = y, label = label, size = label_text_size)
  }
  return(gg_plot)

}

plots_to_pdf <-
  function(plot_objects,
           filename,
           paper,
           height,
           width) {
    # @formatter:off
    #' Creates a pdf with a list of plotted objects
    #' @param all_graph_plots A list of plots
    #' @param filename A string
    #' @param paper A string
    #' @param height A integer
    #' @param width A integer
    #' @return NULL
    # @formatter:on

    pdf(filename,
        paper = paper,
        height = height,
        width = width)

    for (i in seq_along(plot_objects)) {
      p_object <- plot_objects[[i]]
      if (('ggplot' %in% class(p_object)) |
        ('igraph' %in% class(p_object))) {
        print(p_object)
      }
      else {
        draw(p_object)
      }
    }

    dev.off()
  }
