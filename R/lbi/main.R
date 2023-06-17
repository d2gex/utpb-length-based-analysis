library("logger")
library("readr")
library("ggplot2")
library("LBSPR")
source("config.R")
source("utils.R")
source("lbi/catch_length.R")
source("lbi/utilities_LBI.R")

log_info("--------- Generating LBI for all species---------------")
if (!exists('clean_db_data_tallas')) {
  clean_db_data_tallas <- read_csv_input(file.path(DATA_OUTPUT, 'clean_db_tallas.csv'))
}

specie <- "Trisopterus luscus"
bindwidth <- 1 # cm
col_prefix <- 'X'
catch_at_length <- CatchAtLength$new(clean_db_data_tallas, specie)
catch_at_length$build_length_composition_matrix(bindwidth, col_prefix)

g <- bin_plot(catch_at_length$summary_wide, binwidth=1, "cm")
g
