setwd("../../R") # Unfortunate hack to get testthat working with single files and not R package
source("utils.R")
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
  expect_equal(utm_df, expected_crs_25829, tolerance = 1e-3)

})

test_that("Replace columns from B to A", {


  df_to <- data.frame(
    cond_1 = c(1, 2),
    cond_2 = c(1),
    replacement_1 = c('A', 'B'),
    replacement_2 = c(10, 15))
  df_from <- data.frame(
    cond_1 = c(1, 2, 4, 5, 7),
    cond_2 = c(1, 2, 4, 5, 7),
    replacement_1 = c('G', 'H', 'I', 'J', 'K'),
    replacement_2 = c(20, 35, 30, 35, 40),
    non_replacement = seq(1:5))

  expected_merge <- data.frame(
    cond_1 = c(1, 2),
    cond_2 = c(1),
    replacement_1 = c('G', NA),
    replacement_2 = c(20, NA))

  replaced_columns <- c('replacement_1', 'replacement_2')
  condition_columns <- c('cond_1', 'cond_2')
  result <- replace_columns(df_to, df_from, replaced_columns, condition_columns)
  expect_true(!('non_replacement' %in% colnames(result)))
  expect_equal(result, expected_merge)
})

test_that("Generate correct year intervals between two dates", {

  day <- 1
  month <- 1
  date_sep <- '/'
  start_year <- 1999
  end_year <- 2023
  year_step <- 4
  # 1999 2003 2007 2011 2015 2019 2023
  expected_result <- list (
    c(paste0(day, date_sep, month, date_sep, 1999), paste0(day, date_sep, month, date_sep, 2003)),
    c(paste0(day + 1, date_sep, month, date_sep, 2003), paste0(day, date_sep, month, date_sep, 2007)),
    c(paste0(day + 1, date_sep, month, date_sep, 2007), paste0(day, date_sep, month, date_sep, 2011)),
    c(paste0(day + 1, date_sep, month, date_sep, 2011), paste0(day, date_sep, month, date_sep, 2015)),
    c(paste0(day + 1, date_sep, month, date_sep, 2015), paste0(day, date_sep, month, date_sep, 2019)),
    c(paste0(day + 1, date_sep, month, date_sep, 2019), paste0(day, date_sep, month, date_sep, 2023))
  )
  result <- build_yearly_date_intervals(day, month, date_sep, start_year, end_year, year_step)
  expect_equal(result, expected_result)

})