library("R6")
library("data.table")
library("dplyr")
library("assertr")
library("tidyverse")
library("stringi")
library("readr")
source("utils.R")

DbDataFilter <-
  R6Class("DbDataFilter",
          inherit = BaseDataFilter,
          public = list(
            initialize = function(clean_data, dirty_data) {
              super$initialize(clean_data, dirty_data)
            },
            rename_columns = function(old_cols, new_cols) {
              data.table::setnames(self$clean_df, old = old_cols, new = new_cols)
              invisible(self)
            },
            to_datetime = function(fields) {
              self$clean_df <- self$clean_df %>% mutate_at(.vars = fields,
                                                           ~as.POSIXct(., format = "%d/%m/%Y %H:%M", tz = "UTC"))
              invisible(self)
            },
            get_rid_of_NaNs_for_all_cols = function(fields) {

              # Get rid of ALL rows which passed columns do have their values to NaN
              # @param fields an array of strings
              dirty_data <- self$clean_df %>% filter(if_all(fields, ~is.na(.)))
              self$add_to_dirty(dirty_data, error_description = paste('NaN-ALL (excluded):', paste(fields, collapse = ',')))
              self$clean_df <- self$clean_df %>% filter(if_any(fields, ~!is.na(.)))
              invisible(self)
            },
            extract_largada_virada_dates = function() {
              # Build largada and virada times depending on the columns HorafL, HorafV, FLARG and FVIR
              # It guesses potential swapping times and correct them. This function assumes that one of the
              # two fields for largada or virada do have at least a non NaN value.
              self$clean_df <- self$clean_df %>%
                # Get non NA value of the two largada fields; Otherwise the smallest of the two
                mutate(largada_time = case_when(
                  not_na(FLARG) ~ FLARG,
                  not_na(HorafL) ~ HorafL,
                  HorafL <= FLARG ~ HorafL,
                  .default = FLARG
                )) %>%
                # Get either non NA value of the two virada fields; Otherwise the smallest of the two
                mutate(virada_time = case_when(
                  not_na(FVIR) ~ FVIR,
                  not_na(HorafV) ~ HorafV,
                  HorafV <= FVIR ~ HorafV,
                  .default = FVIR
                )) %>%
                # Now swap largada_time and virada_time should they be all the way around
                mutate(
                  aux_largada = case_when(
                    largada_time <= virada_time ~ largada_time,
                    .default = virada_time
                  )
                ) %>%
                # and .. virada with largada
                mutate(
                  aux_virada = case_when(
                    virada_time >= largada_time ~ virada_time,
                    .default = largada_time
                  )
                ) %>%
                # Finally restore largada_time and virada_time with the correct values
                mutate(
                  largada_time = aux_largada,
                  virada_time = aux_virada
                ) %>%
                # calculate soaktime
                mutate(soak_time = virada_time - largada_time) %>%
                # get rid of aux columns
                select(-c(aux_largada, aux_virada))
              invisible(self)
            },
            adhoc_replacements = function() {
              # Need to covert this specific case not to lose the value once the encoding and transform occur
              db_filter$clean_df <- db_filter$clean_df %>%
                mutate(across(.cols = PUERTO_EMBARQUE,
                              ~ifelse(str_detect(., "Cibrao_Porti"), "San Cribao_Porto de Morás", .))) %>%
                mutate(across(.cols = PUERTO_EMBARQUE,
                              ~ifelse(str_detect(., "Louriz"), "Lourizán (Pontevedra)", .))) %>%
                mutate(across(.cols = valor,
                              ~ifelse(is.na(.), "unknown", .)))
            },
            to_encoding = function(fields, encoding, string_transform) {
              self$clean_df <- self$clean_df %>%
                mutate_at(.vars = fields, ~stri_trans_general(., id = string_transform)) %>%
                mutate_at(.vars = fields, ~iconv(., to = encoding))
              invisible(self)
            },
            classify_seafloor = function(hard_options, mixed_options, default, unknown) {

              dirty_data <- self$clean_df %>% filter(is.na(valor) | !nzchar(valor))
              self$dirty_df <- self$add_to_dirty(dirty_data,
                                                 error_description = paste('NAN ->', unknown, ' (replacement): valor'))

              self$clean_df <- self$clean_df %>%
                mutate(seafloor = case_when(
                  is.na(valor) | !nzchar(valor) ~ unknown, # NaN, zero strings
                  valor %in% unlist(unname(hard_options)) ~ names(hard_options),
                  valor %in% unlist(unname(mixed_options)) ~ names(mixed_options),
                  .default = default # options not selected above
                ))
              invisible(self)
            }
          )
  )