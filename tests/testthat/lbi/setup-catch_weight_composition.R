source("../../../R/lbi/catch_weight_composition.R")
library("R6")

CatchWeightCompositionTest <- R6Class("CatchWeightCompositionTest",
                                      inherit = CatchWeightComposition,
                                      public = list(
                                        initialize = function(data,
                                                              size_col,
                                                              weight_col,
                                                              time_col,
                                                              interval_col,
                                                              midpoint_col) {
                                          super$initialize(data, size_col, weight_col,
                                                           time_col, interval_col, midpoint_col)
                                        },
                                        test_generate_length_intervals = function(bindwidth) {
                                          super$generate_length_intervals(bindwidth)
                                        }
                                      ))

# // @formatter:off
  raw_data <- data.frame(
    year = c(2011,  2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2014, 2014),
    talla = c(27.5, 28.3, 29.1, 27.6, 29,   29,   30,   27.6, 28.4, 28.5, 29.5),
    peso =  c(1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11)
  )
  cut_intervals <- c(
    "(27,28]",
    "(28,29]",
    "(29,30]",
    "(27,28]",
    "(28,29]",
    "(28,29]",
    "(29,30]",
    "(27,28]",
    "(28,29]",
    "(28,29]",
    "(29,30]"
  )
  # // @formatter:on