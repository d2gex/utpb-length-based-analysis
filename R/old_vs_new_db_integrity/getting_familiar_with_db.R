source("config.R")
source("utils.R")
library("openxlsx")
if (!exists('db_data')) {
  db_data_tallas <- read_csv2(DB_TALLAS_PATH, locale = locale(encoding = 'latin1'))
  db_data_capturas <- read_csv2(DB_CAPTURAS_PATH, locale = locale(encoding = 'latin1'))
}

fields <- intersect(colnames(db_data_tallas), colnames(db_data_capturas))
population <- db_data_tallas %>%
  filter(if_all(c('LON inicio', 'LON final', 'LAT inicio', 'LAT final'), ~not_na(.)))

lances_sample <- sample(population$Idlance, 100)
tallas_sample <- db_data_tallas %>%
  filter(Idlance %in% lances_sample)
# select_at(.vars = fields)

capturas_sample <- db_data_capturas %>%
  filter(Idlance %in% lances_sample)
# select_at(.vars = fields)

excel_data <- list(
  tallas = tallas_sample,
  capturas = capturas_sample
)
write.xlsx(excel_data, "../data/sensitive/output/sample_tallas_capturas.xlsx")





