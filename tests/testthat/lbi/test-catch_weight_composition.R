library("stringr")
test_that("Generate size intervals and midpoints while carrying size and weight", {
  # // @formatter:off
  raw_data <- data.frame(
    year = c(2011,  2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2014, 2014),
    talla = c(27.5, 28.3, 29.1, 27.6, 29,   29,   30,   27.6, 28.4, 28.5, 29.5),
    peso =  c(1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11)
  )
  # // @formatter:on
  expected_catch_length <- copy(raw_data)
  expected_catch_length$interval <- factor(cut_intervals)
  c_test <- CatchWeightCompositionTest$new(raw_data,
                                           size_col = 'talla',
                                           weight_col = 'peso',
                                           time_col = 'year',
                                           interval_col = 'interval',
                                           midpoint_col = 'midpoint')
  result <- c_test$test_generate_length_intervals(bindwidth = 1)
  expect_equal(result, expected_catch_length)
})