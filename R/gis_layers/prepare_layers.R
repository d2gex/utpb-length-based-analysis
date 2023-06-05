source("utils.R")
crs_source_4326 <- "+init=epsg:4326"
crs_dest_25829 <- "+init=epsg:25829"

galicia_coast_4326 <-
  read_csv2("../data/sensitive/Galicia_coast.csv",
            col_names = c("lon", "lat"))

galicia_coast_4326 <- galicia_coast_4326 %>%
  mutate(lon = ifelse(lon > -8.85 & lat < 42.1, NA, lon)) %>%
  mutate(lon = ifelse(lon > -8.45 & lat < 42.5, NA, lon))

galicia_coast_25829 <- from_crs_to_crs(galicia_coast_4326,
                                       'lon',
                                       'lat',
                                       crs_source_4326,
                                       crs_dest_25829)

galicia_bathymetry_4326 <-
  read_table("../data/sensitive/Galicia_bathy.txt", col_names = c("lon", "lat", "depth")) %>%
    mutate(depth = ifelse(depth > 0, NA, depth))


write_csv(galicia_coast_25829, "../qgis/output/galicia_coast_25829.csv")
