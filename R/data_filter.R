library("R6")
library("data.table")
library("dplyr")
library("assertr")
library("tidyverse")


DbDataFilter <- R6Class("DbDataFilter",
                        public = list(
                          db_data = NULL,
                          clean_df = data.frame(),
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
                            self$clean_df <- self$clean_df %>% mutate_at(.vars = fields, .funs = dmy_hms)
                            invisible(self)
                          },
                          get_rid_of_NaNs = function(fields) {
                            self$clean_df <- private$copy_or_add(self$clean_df, self$db_data %>% drop_na(any_of(fields)))
                            self$dirty_df <- private$copy_or_add(self$dirty_df, self$db_data %>% filter(if_any(fields, ~is.na(.x))))
                            invisible(self)
                          }
                        ),
                        private = list(
                          copy_or_add = function(df_to, df_from) {
                            if (nrow(df_to)) {
                              df_to <- df_from
                            }
                            else {
                              df_to <- rbind(df_to, df_from)
                            }
                            return(df_to)
                          }
                        )
)