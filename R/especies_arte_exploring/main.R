source("config.R")
source("especies_arte_exploring/report.R")
library("openxlsx")
library("ggplot2")
library("stringr")


if (!exists('db_data_tallas')) {
  db_data_tallas <- read_csv2(DB_TALLAS_PATH, locale = locale(encoding = 'latin1'))
  # db_data_capturas <- read_csv2(DB_CAPTURAS_PATH, locale = locale(encoding = 'latin1'))
}

esp_arte_report <- EspeciesArteReport$new(db_data_tallas)
esp_arte_report$generate_summary()
esp_arte_report$add_arte_nicknames()
write_csv(esp_arte_report$summary, "../data/sensitive/output/especies_arte_sampling.csv")

db_data <- copy(esp_arte_report$summary)
up_to_80 <- db_data %>% filter(especie_cum <= 80)
from_80 <- db_data %>% filter(especie_cum > 80)



