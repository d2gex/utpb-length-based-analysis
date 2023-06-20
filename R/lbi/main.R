library("logger")
library("readr")
library("ggplot2")
library("LBSPR")
library("ReporteRsjars")
library("ReporteRs")
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
mean_weight_col <- 'mean_weight'
interval_col <- 'interval'
midpoint_col <- 'MeanLength'
freq_col <- 'catch'
col_prefix <- 'X'

species_data_composition <- SpeciesDataComposition$new(clean_db_data_tallas,
                                                       specie,
                                                       gears,
                                                       time_col,
                                                       size_col,
                                                       weight_col,
                                                       mean_weight_col,
                                                       interval_col,
                                                       midpoint_col,
                                                       freq_col)

species_data_composition$build_talla_and_weight_composition_matrices(bindwidth, col_prefix, min_padding = 1)
catch_at_length_w.ignored_summaries <- species_data_composition$build_talla_only_composition_matrix(bindwidth,
                                                                                                    col_prefix,
                                                                                                    min_padding = 1)

# catch_at_length$build_talla_and_weight_composition_matrices(bindwidth, col_prefix)
t.luscus_lhp <- read_csv(file.path(EXTRA_DATA_PATH, 'species_lh_parameters.csv')) %>%
  filter(stocks == "Trisopterus luscus")

catch_at_length_w.ignored_plot <- bin_plot(catch_at_length_w.ignored_summaries$wide, binwidth = 1, "cm")
catch_at_length_w.notna_plot <- bin_plot(species_data_composition$catch_wide, binwidth = 1, "cm")
traffic_light_plot <- lb_tableSH(data = species_data_composition$catch_wide,
                                 binwidth = 1,
                                 l_units = "cm",
                                 linf = t.luscus_lhp$Linf,
                                 lmat = t.luscus_lhp$L50,
                                 mk_ratio = t.luscus_lhp$M_K,
                                 weight = species_data_composition$mean_weight_wide)


