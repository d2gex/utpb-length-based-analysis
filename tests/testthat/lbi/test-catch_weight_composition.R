library("stringr")
test_that("Size interval and midpoint sequences", {
  expected_intervals <- c(27, 28, 29, 30)
  expected_midpoints <- c(27.5, 28.5, 29.5)
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint',
                                           freq_col = 'freq')
  result <- c_test$test_generate_interval_and_midpoint_sequences(bindwidth = 1)
  expect_equal(result$size_intervals, expected_intervals)
  expect_equal(result$mid_points, expected_midpoints)

})
test_that("Size interval for a given dataframe", {
  expected_catch_length <- copy(raw_data)
  expected_catch_length$interval <- factor(cut_intervals)
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint',
                                           freq_col = 'freq')
  inter_midpoints <- c_test$test_generate_interval_and_midpoint_sequences(bindwidth = 1)
  result <- c_test$test_generate_length_intervals(inter_midpoints$size_intervals)
  expect_equal(result, expected_catch_length)
})


test_that("Catch at length for a given dataframe", {

  # // @formatter:off
  expected_catch_length <- data.frame(
    year =      c(2011, 2011, 2011, 2012, 2012, 2012, 2013, 2013, 2013, 2014, 2014, 2014),
    midpoint =  c(27.5, 28.5, 29.5, 27.5, 28.5, 29.5, 27.5, 28.5, 29.5, 27.5, 28.5, 29.5),
    freq =      c(1,    1,    1,    1,    1,    0,    0,    1,    1,    1,    2,    1)
  )
  # // @formatter:on
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint',
                                           freq_col = 'freq')
  inter_midpoints <- c_test$test_generate_interval_and_midpoint_sequences(bindwidth = 1)
  result <- c_test$test_generate_catch_at_length_frequency(inter_midpoints$size_intervals, inter_midpoints$mid_points)
  expect_equal(result %>% select(year, midpoint, freq), expected_catch_length)

})
test_that("Mean weight at length for a given dataframe", {
  # // @formatter:off
    expected_mean_weight <- data.frame(
    year =      c(2011, 2011, 2011,
                  2012, 2012,
                  2013, 2013,
                  2014, 2014, 2014),
    interval =  factor(c("(27,28]", "(28,29]", "(29,30]",
                  "(27,28]", "(28,29]",
                  "(28,29]", "(29,30]",
                  "(27,28]", "(28,29]", "(29,30]")),
    mean_weight =      c(1, 2, 3,
                  4, 5,
                  6, 7,
                  8, 9.5, 11)
  )
  # // @formatter:on
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint',
                                           freq_col = 'freq')
  inter_midpoints <- c_test$test_generate_interval_and_midpoint_sequences(bindwidth = 1)
  size_intervas_weight_df <- c_test$test_generate_length_intervals(inter_midpoints$size_intervals)
  result <- c_test$test_generate_mean_weight_at_length(size_intervas_weight_df)
  expect_equal(result, expected_mean_weight)
})

