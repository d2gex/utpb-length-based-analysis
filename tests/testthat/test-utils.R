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
