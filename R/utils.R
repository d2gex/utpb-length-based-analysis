library("R6")

BaseDataFilter <- R6Class("BaseDataFilter", public = list(
  db_data = NULL,
  clean_df = NULL,
  dirty_df = data.frame(),
  initialize = function(clean_data, dirty_data) {
    self$db_data <- clean_data
    self$clean_df <- data.frame(clean_data)
    if (!missing(dirty_data)) {
      self$dirty_df <- data.frame(dirty_data)
    }
  },
  add_to_dirty = function(df, error_description) {
    # Add error description to dirty_df
    df <- df %>%
      mutate(error = error_description)
    if (nrow(self$dirty_df) == 0) {
      self$dirty_df <- data.frame(df)
    }
    else {
      new_columns <- setdiff(names(df), names(self$dirty_df))
      self$dirty_df[new_columns] <- 'artifitcially created'
      self$dirty_df <- rbind(self$dirty_df, df)
    }
  },
  add_to_clean = function(df) {
    self$clean_df <- rbind(self$clean_df, df)
  },
  set_clean_data = function (df) {
    self$clean_df <- data.frame(df)
  },
  set_dirty_data = function (df) {
    self$dirty_df <- data.frame(df)
  }
))