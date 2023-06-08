library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")
source("config.R")
source("utils.R")
source("especies_arte_exploring/utils.R")
source("especies_arte_exploring/report.R")


if (!exists('db_data_tallas')) {
  db_data_tallas <- read_csv2(DB_TALLAS_PATH, locale = locale(encoding = 'latin1'))
  # db_data_capturas <- read_csv2(DB_CAPTURAS_PATH, locale = locale(encoding = 'latin1'))
}

esp_arte_report <- EspeciesArteReport$new(db_data_tallas, 4)
esp_arte_report$generate_summary()
esp_arte_report$add_arte_nicknames()
write_csv(esp_arte_report$summary, "../data/sensitive/output/especies_arte_sampling.csv")

db_data <- copy(esp_arte_report$summary)
up_to_80 <- db_data %>% filter(especie_cum <= 80)
from_80 <- db_data %>% filter(especie_cum > 80)

#-----------------------------------------------------------------
#           Build and plot 80-80 rule for especies and ARTE
#-----------------------------------------------------------------
summary_especie_arte <- esp_arte_report$get_most_representative_arte(up_to_80, threshold = 80, other_keyword = "Other")
g <- plot_especies_arte_barplot(summary_especie_arte,
                                title = "80%-80% rule: Most contributing species and gears",
                                x_lab = "Species",
                                y_lab = "Species contribution to the sampling(%)",
                                legend_title = 'Gears',
                                x_angle = 45,
                                vertical_adjusment_func = ceiling)
g

#-----------------------------------------------------------------
#           Build and plot 80-20 rule for especies and ARTE
#-----------------------------------------------------------------
summary_especie_arte <- esp_arte_report$get_most_representative_arte(from_80, threshold = 80, other_keyword = "Other")
g <- plot_especies_arte_barplot(summary_especie_arte,
                                title = "20%-80% rule: Less contributing species and most contributed gears",
                                x_lab = "Species",
                                y_lab = "Species contribution to the sampling(%)",
                                legend_title = 'Gears',
                                x_angle = 90,
                                vertical_adjusment_func = function(x) round(x, 1) + 0.1)
g






