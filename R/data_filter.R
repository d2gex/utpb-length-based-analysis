library("R6")
library("data.table")


DbDataFilter <- R6Class("DbDataFilter",
                        public = list(
                          db_data = NULL,
                          initialize = function(df) {
                            self$db_data <- df
                          },
                          rename_columns = function(old_cols, new_cols) {
                            data.table::setnames(self$db_data, old=old_cols, new=new_cols)
                            invisible(self)
                          }
                        ))