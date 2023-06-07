source("../utils.R")
library("R6")
library("sf")

LongLatFilter <- R6Class("LongLatFilter",
                         inherit = BaseDataFilter,
                         public = list(
                           initialize = function(clean_data, dirty_data) {
                             super$initialize(clean_data, dirty_data)
                           },
                           get_rid_of_NaNs = function(long_fields, lat_fields) {

                             # Report any row that has got one pair of geo points as NA
                             dirty_df <- self$clean_df %>%
                               filter(if_all(long_fields, ~is.na(.)) | if_all(lat_fields, ~is.na(.)))
                             error_description <- paste('NAN-ALL_PAIRS:(excluded)',
                                                        '[',
                                                        paste(long_fields, collapse = ','),
                                                        '], ',
                                                        '[',
                                                        paste(lat_fields, collapse = ','),
                                                        ']'
                             )
                             self$add_to_dirty(dirty_df, error_description = error_description)

                             # Get only those rows where there is at least one value in the pair of log
                             # and lat as non-NA
                             self$clean_df <- self$clean_df %>%
                               filter(if_any(long_fields, ~not_na(.)) & if_any(lat_fields, ~not_na(.)))
                             invisible(self)
                           },
                           to_espg_4326 = function() {

                             self$clean_df <- self$clean_df %>%
                               mutate(
                                 lon = case_when(
                                   is.na(start_long) ~ end_long,
                                   .default = start_long
                                 )
                               ) %>%
                               mutate(
                                 lat = case_when(
                                   is.na(start_lat) ~ end_lat,
                                   .default = start_lat
                                 )
                               ) %>%
                               mutate(lon = private$f_lon(lon)) %>%
                               mutate(lat = private$f_lat(lat))
                           },
                           from_crs_to_crs = function(longitude, latitude, source, dest) {
                             geo_df <- from_crs_to_crs(self$clean_df,
                                                       longitude, latitude, source, dest)
                             self$clean_df <- self$clean_df %>%
                               mutate(lon_utm = geo_df$lon, lat_utm = geo_df$lat)
                           }
                         ),
                         private = list(
                           f_lat = function(x) {
                             trunc(x / 100000) + ((x - (100000 * trunc(x / 100000))) / 1000) / 60
                           },
                           f_lon = function(x) {
                             -(trunc(x / 100000) + ((x - (100000 * trunc(x / 100000))) / 1000) / 60)
                           }
                         ))