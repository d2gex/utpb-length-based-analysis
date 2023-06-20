library("R6")
library("dplyr")
library("tidyr")
library("stringr")
library("assertr")
source("utils.R")
source("lbi/catch_weight_composition.R")

SpeciesDataComposition <- R6Class("SpeciesDataComposition", public = list(
  species_data = NULL,
  species = NULL,
  gears = NULL,
  time_col = NULL,
  size_col = NULL,
  weight_col = NULL,
  mean_weight_col = NULL,
  interval_col = NULL,
  midpoint_col = NULL,
  catch_col = NULL,
  catch_long = NULL,
  catch_wide = NULL,
  mean_weight_long = NULL,
  mean_weight_wide = NULL,
  composition = NULL,

  initialize = function(db_data,
                        species,
                        gears,
                        time_col,
                        size_col,
                        weight_col,
                        mean_weight_col,
                        interval_col,
                        midpoint_col,
                        catch_col) {
    self$species <- species
    self$gears <- gears
    self$time_col <- time_col
    self$size_col <- size_col
    self$weight_col <- weight_col
    self$mean_weight_col <- mean_weight_col
    self$interval_col <- interval_col
    self$midpoint_col <- midpoint_col
    self$catch_col <- catch_col
    private$init(db_data)

  },
  build_talla_only_composition_matrix = function(bindwidth, col_prefix, min_padding) {
    # // @formatter:off
    #' Build a year-basis catch-at-length composition composition matrix when weight is ignored
    # // @formatter:on
    summary_catch <- self$
      composition$
      generate_catch_at_length_composition(bindwidth, min_padding)
    summary_catch_long_wide <-
      private$build_long_wide_variable_composition_matrix(summary_catch,
                                                          col_prefix,
                                                          self$catch_col)
    return(list(long = summary_catch_long_wide$long, wide = summary_catch_long_wide$wide))
  },

  build_talla_and_weight_composition_matrices = function(bindwidth, col_prefix, min_padding) {
    # // @formatter:off
    #' Build a year-basis catch-at-length and mean-weight composition matrices both in long and wide format
    #' (4 matrices, 2 for catch_at_length and 2 for mean-weight)
    # // @formatter:on

    # (1) Generate catch and mean-weight
    catch_mean_weight_at_length <- self$
      composition$
      generate_catch_and_m.weight_at_length_composition(bindwidth, min_padding = 1)

    # (2) Build long and wide catch-at-length dataframe
    catch_details <- catch_mean_weight_at_length %>% select(-!!self$mean_weight_col)
    summary_catch <- private$build_long_wide_variable_composition_matrix(catch_details, col_prefix, self$catch_col)
    self$catch_long <- summary_catch$long
    self$catch_wide <- summary_catch$wide

    # (3) Build long and wide mean-weight-at-length dataframe
    mean_weight_details <- catch_mean_weight_at_length %>% select(-!!self$catch_col)
    to_kg <- function(x) { round(x / 100, 2) }
    summary_mean_weight <- private$build_long_wide_variable_composition_matrix(mean_weight_details,
                                                                               col_prefix,
                                                                               self$mean_weight_col,
                                                                               converter_func = to_kg)
    self$mean_weight_long <- summary_mean_weight$long
    self$mean_weight_wide <- summary_mean_weight$wide
  }

), private = list(

  init = function(db_data) {
    # specific data over which the compotion will be applied
    self$species_data <- db_data %>%
      filter(ESPECIE == self$species) %>%
      filter(ARTE %in% self$gears) %>%
      select_at(.vars = c(self$time_col, self$size_col, self$weight_col))

    # Catch at length and mean weigh composition generator
    self$composition <- CatchWeightComposition$new(
      self$species_data,
      self$size_col,
      self$weight_col,
      self$time_col,
      self$interval_col,
      self$midpoint_col,
      self$catch_col
    )
  }
  ,
  build_long_wide_variable_composition_matrix = function(data,
                                                         col_prefix,
                                                         variable,
                                                         converter_func = NULL) {

    # return long frame
    summary_long <- data %>%
      arrange_at(.vars = c(self$time_col, self$interval_col))
    if (!is.null(converter_func)) {
      summary_long <- summary_long %>%
        mutate_at(.vars = variable, .funs = converter_func)
    }

    # return wide frame
    summary_wide <- summary_long %>%
      pivot_wider(id_cols = -!!self$interval_col, names_from = !!self$time_col, values_from = !!variable)
    summary_wide <- summary_wide %>%
      mutate_at(vars(-self$midpoint_col), replace_na, 0) %>%
      rename_with(~str_c(col_prefix, .x), matches("^\\d{4}$"))

    mute <- summary_wide %>% assert(not_na, colnames(.))
    return(list(long = summary_long, wide = summary_wide))
  }

))


