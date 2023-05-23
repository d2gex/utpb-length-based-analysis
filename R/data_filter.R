library("R6")
library("data.table")
library("dplyr")
library("assertr")
library("tidyverse")


DbDataFilter <-
  R6Class("DbDataFilter",
          public = list(
            db_data = NULL,
            clean_df = NULL,
            dirty_df = data.frame(),
            initialize = function(df) {
              self$db_data <- df
              self$clean_df <- data.frame(df)
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
                                          #' Get rid of *all* rows which passed columns do have their values to NaN
                                          #' @param fields an array of strings
              dirty_data <- self$clean_df %>% filter(if_all(fields, ~is.na(.)))
              self$dirty_df <- private$copy_or_add(self$dirty_df, dirty_data)
              self$clean_df <- self$clean_df %>% filter(if_any(fields, ~!is.na(.)))
              invisible(self)
            },
            extract_largada_virada_dates = function() {
                                          #' Build largada and virada times depending on the columns HorafL, HorafV, FLARG and FVIR
                                          #' It guesses potential swapping times and correct them. This function assumes that one of the
                                          #' two fields for largada or virada do have at least a non NaN value.
              db_filter$clean_df <- db_filter$clean_df %>%
                # Get either non NA value of the two largada fields; Otherwise the smallest of the two
                mutate(largada_time = case_when(
                  is.na(HorafL) ~ FLARG,
                  is.na(FLARG) ~ HorafL,
                  HorafL <= FLARG ~ HorafL,
                  .default = FLARG
                )) %>%
                # Get either non NA value of the two virada fields; Otherwise the smallest of the two
                mutate(virada_time = case_when(
                  is.na(HorafV) ~ FVIR,
                  is.na(FVIR) ~ HorafV,
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

            }
          ),
          private = list(
            copy_or_add = function(df_to, df_from) {
              if (!nrow(df_to)) {
                df_to <- df_from
              }
              else {
                df_to <- rbind(df_to, df_from)
              }
              return(df_to)
            }
          )
  )