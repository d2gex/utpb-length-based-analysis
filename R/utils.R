library("data.table")
library("sf")

get_column_data_type <- function(df, column_name) {
  return(
    sapply(df, class)[column_name]
  )
}

make_cols_same_length <- function(col_1, col_2, fill) {
                        #' Ensure to given columns have the same length by filling up the shotest columns with a give
                        #' filling value

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

add_text_top_every_bar <- function(gg_plot, df, x_col, y_col, label_col, label_text_size, vertical_adjustment_function) {
  # @formatter:off
  #' Add the value of one of the collums in the passed dataframe on top of each bar. The vertical adjustment
  #' is calculated by the passed function
  # @formatter:on

  for (row in 1:nrow(df)) {
    x <- df[row, x_col]
    y <- vertical_adjustment_function(df[row, y_col])
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

create_empty_dataframe <- function(col_names) {
  df <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
  colnames(df) <- col_names
  return(df)
}