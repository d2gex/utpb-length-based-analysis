library("R6")
library("dplyr")
library("tidyr")
library("stringr")
source("utils.R")

CatchAtLength <- R6Class("CatchAtLength", public = list(
  species_data = NULL,
  summary_long = NULL,
  summary_wide = NULL,
  initialize = function(db_data, specie, gears) {
    self$species_data <- db_data %>%
      filter(ESPECIE == specie) %>%
      filter(ARTE %in% gears) %>%
      select(year, TALLA)
  },

  build_length_composition_matrix = function(bind_width, col_prefix) {
    catch_at_length <- generate_catch_at_length_freq_table(self$species_data,
                                                           bind_width,
                                                           variable = 'TALLA',
                                                           reference = 'year')
    catch_at_length <- catch_at_length %>%
      arrange(year)
    self$summary_long <- catch_at_length
    self$summary_wide <- self$summary_long %>%
      pivot_wider(id_cols = -interval, names_from = year, values_from = freq) %>%
      mutate_at(vars(-midpoint), replace_na, 0) %>%
      rename_with(~str_c(col_prefix, .x), matches("^\\d{4}$"))

  }
))


