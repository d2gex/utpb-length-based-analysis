library("R6")
library("dplyr")
library("tidyr")
library("stringr")
source("utils.R")

CatchAtLength <- R6Class("CatchAtLength", public = list(
  species_data = NULL,
  species = NULL,
  gears = NULL,
  size_col = NULL,
  weight_col = NULL,
  # catch at length composition when the column weight is not taking into account
  talla_w.ignore_long = NULL,
  # catch at length composition when the column weight must have a non-na value
  talla_w.notna_long = NULL,
  talla_w.notna_wide = NULL,
  # mean weight composition
  weight_long = NULL,
  weight_wide = NULL,

  initialize = function(db_data, species, gears, time_col, size_col, weight_col) {
    self$species <- species
    self$gears <- gears
    self$time_col <- time_col
    self$size_col <- size_col
    self$weight_col <- weight_col
    self$species_data <- db_data %>%
      filter(ESPECIE == self$species) %>%
      filter(ARTE %in% self$gears) %>%
      select_at(.vars = c(self$time_col, self$size_col, self$weight_col))
  },
  build_talla_and_weight_composition_matrices = function(bindwidth, col_prefix) {
    # // @formatter:off
    #' Build the year-basis composition matrices for size and weight
    # // @formatter:on

    talla_w.ignore_data <- self$species_data %>%
      select(TALLA, year)
    talla_w.notna_data <- self$species_data %>%
      filter(if_all(c('TALLA', 'PESO'), ~not_na(.))) %>%
      select(TALLA, year)
    weight_data <- self$species_data %>%
      filter(if_all(c('TALLA', 'PESO'), ~not_na(.))) %>%
      select(PESO, year)
    mute <- talla_w.notna_data %>% verify(nrow(.) == nrow(weight_data))
    talla_w.ignore_summaries <- private$build_variable_composition_matrix(talla_w.ignore_data,
                                                                          bindwidth,
                                                                          col_prefix,
                                                                          'TALLA',
                                                                          'year')
    talla_w.notna_summaries <- private$build_variable_composition_matrix(talla_w.notna_data,
                                                                         bindwidth,
                                                                         col_prefix,
                                                                         'TALLA',
                                                                         'year')
    weight_summaries <- private$build_variable_composition_matrix(weight_data,
                                                                  bindwidth,
                                                                  col_prefix,
                                                                  'PESO',
                                                                  'year')
    self$talla_w.ignore_long <- talla_w.ignore_summaries$long
    self$talla_w.ignore_wide <- talla_w.ignore_summaries$wide
    self$talla_w.notna_long <- talla_w.notna_summaries$long
    self$talla_w.notna_wide <- talla_w.notna_summaries$wide
    self$weight_long <- weight_summaries$long
    self$weight_wide <- weight_summaries$wide


  }

), private = list(
  build_variable_composition_matrix = function(data, bind_width, col_prefix, variable,) {
    # // @formatter:off
    #' Build the year-basis composition matrix for a given variable (size or weight) and
    #' reference (typically 'year')
    # // @formatter:on
    variable_composition <- generate_catch_at_length_freq_table(data,
                                                           bind_width,
                                                           variable,
                                                           reference)
    variable_composition <- variable_composition %>%
      arrange_at(self$time_col)
    summary_long <- variable_composition

    summary_wide <- summary_long %>%
      pivot_wider(id_cols = -interval, names_from = year, values_from = freq) %>%
      mutate_at(vars(-midpoint), replace_na, 0) %>%
      rename_with(~str_c(col_prefix, .x), matches("^\\d{4}$"))
    mute <- summary_wide %>% assert(not_na, colnames(.))
    return(list(long = summary_long, wide = summary_wide))
  }

))


