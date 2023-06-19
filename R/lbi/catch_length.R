library("R6")
library("dplyr")
library("tidyr")
library("stringr")
source("utils.R")

CatchAtLength <- R6Class("CatchAtLength", public = list(
  species_data = NULL,
  # length composition when weight column is ignored and when weight is checked to be non-NA
  talla_w.ignore_long = NULL,
  talla_w.notna_long = NULL,
  talla_w.ignore_wide = NULL,
  talla_w.notna_wide = NULL,
  # weight composition
  weight_long = NULL,
  weight_wide = NULL,
  initialize = function(db_data, specie, gears) {
    self$species_data <- db_data %>%
      filter(ESPECIE == specie) %>%
      filter(ARTE %in% gears)
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
  build_variable_composition_matrix = function(data, bind_width, col_prefix, variable, reference) {
    # // @formatter:off
    #' Build the year-basis composition matrix for a given variable (size or weight) and
    #' reference (typically 'year')
    # // @formatter:on
    catch_at_length <- generate_catch_at_length_freq_table(data,
                                                           bind_width,
                                                           variable = variable,
                                                           reference = reference)
    catch_at_length <- catch_at_length %>%
      arrange(year)
    summary_long <- catch_at_length
    summary_wide <- summary_long %>%
      pivot_wider(id_cols = -interval, names_from = year, values_from = freq) %>%
      mutate_at(vars(-midpoint), replace_na, 0) %>%
      rename_with(~str_c(col_prefix, .x), matches("^\\d{4}$"))
    mute <- summary_wide %>% assert(not_na, colnames(.))
    return(list(long = summary_long, wide = summary_wide))
  }

))


