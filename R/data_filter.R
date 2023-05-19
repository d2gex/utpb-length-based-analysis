library("R6")

DbDataFilter <- R6Class("DbDataFilter",
                        public = list(
                          db_data = NULL,
                          initialize = function(df) {
                            self$db_data <- df
                          },
                          rename_columns = function(old_cols, new_cols) {
                            self$db_data <- self$db_data %>% rename(setNames(old_cols, new_cols))
                            invisible(self)
                          }
                        ))