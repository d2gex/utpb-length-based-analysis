source("config.R")
source("utils.R")
library("openxlsx")
library("assertr")
library("readr")
library("dplyr")

if (!exists('db_data_tallas')) {
  db_data_tallas <- read_csv2(DB_TALLAS_PATH, locale = locale(encoding = 'latin1'))
  db_data_capturas <- read_csv2(DB_CAPTURAS_PATH, locale = locale(encoding = 'latin1'))
}


# ---------------------------------------------------------------------
#             Exploring capturas and tallas
# ---------------------------------------------------------------------
# fields <- intersect(colnames(db_data_tallas), colnames(db_data_capturas))
# population <- db_data_tallas %>%
#   filter(if_all(c('LON inicio', 'LON final', 'LAT inicio', 'LAT final'), ~not_na(.)))
#
# lances_sample <- sample(population$Idlance, 100)
# tallas_sample <- db_data_tallas %>%
#   filter(Idlance %in% lances_sample)
# # select_at(.vars = fields)
#
# capturas_sample <- db_data_capturas %>%
#   filter(Idlance %in% lances_sample)
# # select_at(.vars = fields)
#
# excel_data <- list(
#   tallas = tallas_sample,
#   capturas = capturas_sample
# )
# write.xlsx(excel_data, "../data/sensitive/output/sample_tallas_capturas.xlsx")


# ---------------------------------------------------------------------
#             Exploring especies and arte
# ---------------------------------------------------------------------
esp_arte_rows <- db_data_tallas %>%
  group_by(ESPECIE, ARTE) %>%
  summarise(num_rows_arte_especies = n())

esp_arte_individuos <- db_data_tallas %>%
  filter(not_na(NUMINDIVS)) %>%
  group_by(ESPECIE, ARTE) %>%
  summarise(num_ind_arte_especie = sum(NUMINDIVS)) %>%
  mutate(num_ind_especie = sum(num_ind_arte_especie),
         arte_especie_fraction = round(num_ind_arte_especie / sum(num_ind_arte_especie), 2) * 100) %>%
  arrange(ESPECIE, desc(arte_especie_fraction)) %>%
  mutate(arte_especie_cum = cumsum(arte_especie_fraction)) %>%
  ungroup() %>%
  mutate(especie_fraction = round(num_ind_especie / sum(num_ind_arte_especie), 2) * 100)

especies_cumsum <- esp_arte_individuos %>%
  distinct(ESPECIE, especie_fraction, .keep_all = TRUE) %>%
  arrange(desc(especie_fraction)) %>%
  mutate(especie_cum = round(cumsum(especie_fraction)), 2) %>%
  select(ESPECIE, especie_cum)


mute <- esp_arte_individuos %>% # Ensure there are no NAs in the rows
  assert(not_na, colnames(.))

especies_arte_abundance_data <- merge(esp_arte_rows,
                                      esp_arte_individuos,
                                      by = c("ESPECIE", "ARTE"),
                                      all = TRUE) %>%
  select(ESPECIE, ARTE, num_rows_arte_especies, num_ind_arte_especie, arte_especie_fraction,
         arte_especie_cum, especie_fraction)

especies_arte_abundance_data <- merge(especies_arte_abundance_data, especies_cumsum, by = "ESPECIE", all = TRUE) %>%
  arrange(desc(especie_fraction), ESPECIE, desc(arte_especie_fraction))

write_csv(especies_arte_abundance_data, "../data/sensitive/output/especies_arte_sampling.csv")

