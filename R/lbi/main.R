library("logger")
library("readr")
library("ggplot2")
library("LBSPR")
source("config.R")
source("utils.R")
source("lbi/species_data_composition.R")
source("lbi/utilities_LBI.R")

log_info("--------- Generating LBI for all species---------------")
if (!exists('clean_db_data_tallas')) {
  clean_db_data_tallas <- read_csv_input(file.path(DATA_OUTPUT, 'clean_db_tallas.csv'))
}

specie <- "Trisopterus luscus"
gears <- "VETAS"
bindwidth <- 1 # cm
time_col <- 'year'
size_col <- 'TALLA'
weight_col <- 'PESO'
col_prefix <- 'X'
catch_at_length <- SpeciesDataComposition$new(clean_db_data_tallas, specie, gears, time_col, size_col, weight_col)
result <- catch_at_length$build_talla_only_composition_matrix(bindwidth = bindwidth)
result


# catch_at_length$build_talla_and_weight_composition_matrices(bindwidth, col_prefix)
# t_g <- bin_plot(catch_at_length$talla_w.ignore_wide, binwidth = 1, "cm")
# t.luscus_lhp <- read_csv(file.path(EXTRA_DATA_PATH, 'species_lh_parameters.csv')) %>%
#   filter(stocks == "Trisopterus luscus")
# traffic_light <- lb_tableSH(data = catch_at_length$talla_w.notna_wide,
#                             binwidth = 1,
#                             l_units = "cm",
#                             linf = t.luscus_lhp$Linf,
#                             lmat = t.luscus_lhp$L50,
#                             mk_ratio = t.luscus_lhp$M_K,
#                             weight = catch_at_length$weight_wide)
# t_g
# traffic_light