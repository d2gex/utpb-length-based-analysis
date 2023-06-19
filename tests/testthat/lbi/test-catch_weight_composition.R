library("stringr")
test_that("Size interval and midpoint sequences", {
  expected_intervals <- c(27, 28, 29, 30)
  expected_midpoints <- c(27.5, 28.5, 29.5)
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint')
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
                                           midpoint_col = 'midpoint')
  inter_midpoints <- c_test$test_generate_interval_and_midpoint_sequences(bindwidth = 1)
  result <- c_test$test_generate_length_intervals(inter_midpoints$size_intervals)
  expect_equal(result, expected_catch_length)
})

