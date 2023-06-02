source("../../R/utils.R")
library("data.table")

test_that("Balance out INTEGER dataframe with different length", {
  df_x <- data.frame(
    integer = c(1, 2, 3)
  )
  df_y <- data.frame(
    integer = c(1, 2, 3, 4)
  )
  fill <- -Inf
  results <- make_cols_same_length(df_x$integer, df_y$integer, fill)
  expect_equal(results$col_1, c(df_x$integer, fill))
  expect_equal(results$col_2, df_y$integer)
})

test_that("Balance out STRING dataframe with different length", {
  df_x <- data.frame(
    str = c('str1', 'str2', 'str3')
  )
  df_y <- data.frame(
    str = c('str1', 'str2')
  )
  fill <- '-Inf'
  results <- make_cols_same_length(df_x$str, df_y$str, fill)
  expect_equal(results$col_1, df_x$str)
  expect_equal(results$col_2, c(df_y$str, fill))
})

test_that("Do nothing for dataframes of equal length", {
  df_x <- data.frame(
    str = c('str1', 'str2')
  )
  df_y <- data.frame(
    str = c('str1', 'str2')
  )
  fill <- '-Inf'
  results <- make_cols_same_length(df_x$str, df_y$str, fill)
  expect_equal(results$col_1, df_x$str)
  expect_equal(results$col_2, df_y$str)
})

test_that("Convert from CRS to CRS", {
  crs_4326 <- data.frame(
    longitude = c(-8.21625, -8.21627, -8.21392),
    latitude = c(41.9000, 41.9001, 41.9159)
  )
  expected_crs_25829 <- data.frame(
    longitude = c(4638970.39, 4638981.48, 4638981.51),
    latitude = c(565010.0596, 565008.2992, 565187.1500)
  )
  crs_source_4326 <- "+init=epsg:4326"
  crs_dest_25829 <- "+init=epsg:25829"
  utm_df <- from_crs_to_crs(crs_4326, 'longitude', 'latitude', crs_source_4326, crs_dest_25829)
  expect_equal(utm_df, expected_crs_25829, tolerance=1e-3)

})

