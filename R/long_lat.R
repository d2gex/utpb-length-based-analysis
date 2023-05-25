library("R6")
source("utils.R")

LongLatFilter <- R6Class("LongLatFilter",
                         inherit = BaseDataFilter,
                         public = list(
                           initialize = function(df) {
                             super$initialize(df)
                           },
                           get_rid_of_NaNs = function(long_fields, lat_fields) {

                             # Report any row that has got one pair of geo points as NA
                             dirty_df <- self$clean_df %>%
                               filter(if_all(long_fields, ~is.na(.)) | if_all(lat_fields, ~is.na(.)))
                             self$add_to_dirty(dirty_df, error_description = paste('Pair long/Lat=NA'))

                             # Get only those rows where there is at least one value in the pair of log
                             # and lat as non-NA
                             self$clean_df <- self$clean_df %>%
                               filter(if_any(long_fields, ~not_na(.)) & if_any(lat_fields, ~not_na(.)))
                             invisible(self)
                           }
                         ))