source("config.R")
source("especies_arte_exploring/report.R")
library("openxlsx")
library("ggplot2")
library("stringr")


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


# Get all ARTEs per ESPECIE which total cumulative sum(%) is above a threshold
closes_to_80 <- up_to_80 %>%
  select(ESPECIE, ARTE, arte_especie_fraction, arte_especie_cum) %>%
  group_by(ESPECIE) %>%
  filter(arte_especie_cum > 80) %>%
  filter(arte_especie_cum - 80 == min(arte_especie_cum - 80)) %>%
  rename(min_cum_threshold_arte_especie = arte_especie_cum) %>%
  select(-c(arte_especie_fraction, ARTE))

# Rename those ARTEs that lay within (100 - threshold)% as 'Others'
up_to_80 <- merge(up_to_80, closes_to_80, by = "ESPECIE", all = TRUE) %>%
  mutate(ARTE = case_when(
    arte_especie_cum > min_cum_threshold_arte_especie ~ 'Others',
    .default = ARTE
  ), arte_nickname = case_when(
    ARTE == 'Others' ~ 'Others',
    .default = arte_nickname
  )) %>%
  arrange(desc(especie_fraction), ESPECIE, desc(arte_especie_fraction))


# Split main ARTEs from remaining
main_ARTE <- up_to_80 %>%
  filter(ARTE != 'Others') %>%
  select(ESPECIE, ARTE, arte_nickname, arte_especie_fraction, especie_fraction)

remaining_arte <- up_to_80 %>%
  filter(ARTE == 'Others') %>%
  select(ESPECIE, ARTE, arte_nickname, arte_especie_fraction, especie_fraction) %>%
  group_by(ESPECIE) %>%
  mutate(arte_especie_fraction = sum(arte_especie_fraction)) %>%
  distinct()


summary_up_to_80 <- rbind(main_ARTE, remaining_arte) %>%
  mutate(arte_especie_absolute_fraction = (arte_especie_fraction / 100) * especie_fraction) %>%
  arrange(-arte_especie_absolute_fraction, ESPECIE)

ggplot(summary_up_to_80,
       aes(fill = ARTE,
           x = reorder(ESPECIE, -arte_especie_absolute_fraction),
           y = arte_especie_absolute_fraction)) +
         geom_bar(position = "stack", stat = "identity") +
         xlab('Species') +
         ylab('Total contribution to the sampling(%)') +
         ggtitle("Species-Gears following 80-80(%) rule")


