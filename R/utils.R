library("R6")

BaseDataFilter <- R6Class("BaseDataFilter", public = list(
  db_data = NULL,
  clean_df = NULL,
  dirty_df = data.frame(),
  initialize = function(df) {
    self$db_data <- df
    self$clean_df <- data.frame(df)
  },
  add_to_dirty = function(dirty_df, error_description) {
    # Add error description to dirty_df
    dirty_df <- dirty_df %>%
      mutate(error = error_description)
    if (nrow(self$dirty_df) == 0) {
      self$dirty_df <- data.frame(dirty_df)
    }
    else {
      new_columns <- setdiff(names(dirty_df), names(self$dirty_df))
      self$dirty_df[new_columns] <- 'artifitcially created'
      self$dirty_df <- rbind(self$dirty_df, dirty_df)
    }
  }
))