library("R6")

CatchWeightComposition <- R6Class("CatchWeightComposition",
                                  public = list(
                                    data = NULL,
                                    composition = NULL,
                                    size_col = NULL,
                                    weight_col = NULL,
                                    time_col = NULL,
                                    interval_col = NULL,
                                    midpoint_col = NULL,
                                    initialize = function(data, size_col, weight_col, time_col,
                                                          interval_col, midpoint_col) {
                                      self$data <- data
                                      self$size_col <- size_col
                                      self$weight_col <- weight_col
                                      self$time_col <- time_col
                                      self$interval_col <- interval_col
                                      self$midpoint_col <- midpoint_col
                                    }
                                  ),
                                  private = list(
                                    generate_length_intervals = function(bindwidth) {
                                      # // @formatter:off
                                      #' Generate a dataframe organised by size intervals of width 'bindwidth' and
                                      #' its midpoints for each time period in the original dataframe. 'size_col' and
                                      #' 'weight_col' are as well added as columns.
                                      # // @formatter:off
                                      unique_time_periods <- unique(self$data[[self$time_col]])
                                      min <- floor(min(self$data[, self$size_col]))
                                      max <- ceiling(max(self$data[, self$size_col]))
                                      half_bindwidth <- bindwidth / 2
                                      unique_size_intervals <- seq(min, max, bindwidth)
                                      mid_points <- seq(min + half_bindwidth, max - half_bindwidth, bindwidth)
                                      lengt_test <- length(unique_size_intervals) == length(mid_points) + 1
                                      testit::assert(deparse(lengt_test), lengt_test)

                                      columns <- c(self$time_col, self$interval_col, self$midpoint_col,
                                                   self$size_col, self$weight_col)
                                      size_weight_time_df <- create_empty_dataframe(columns)
                                      for (time_period in unique_time_periods) {
                                        yearly_data <- self$data %>%
                                          filter_at(.vars = self$time_col, ~.x == time_period) %>%
                                          select_at(.vars = c(self$time_col, self$size_col, self$weight_col))
                                        intervals <- cut(yearly_data[[self$size_col]], unique_size_intervals)
                                        yearly_data <- yearly_data %>%
                                          mutate(
                                            !!columns[2] := intervals
                                          )
                                        size_weight_time_df <- rbind(size_weight_time_df, yearly_data)
                                      }
                                      return(size_weight_time_df)
                                    }
                                  ))