source("config.R")
source("especies_arte_exploring/report.R")
library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")


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
to_plot_df <- summary_especie_arte %>% select(ESPECIE, arte_especie_absolute_fraction, ARTE, num_ind_especie)
num_especie_individuals <- summary_especie_arte %>%
  select(ESPECIE, num_ind_especie, especie_fraction) %>%
  distinct()

g <- to_plot_df %>% ggplot(
  aes(fill = ARTE,
      x = reorder(ESPECIE, -arte_especie_absolute_fraction),
      y = arte_especie_absolute_fraction)) +
  geom_bar(position = "stack", stat = "identity")

# Unfortunately each element on the x-axis may have different categories so
# it is not possible to use geom_text for topping the bar with number on individuals
for (row in 1:nrow(num_especie_individuals)) {
  x <- num_especie_individuals[row, "ESPECIE"]
  y <- ceiling(num_especie_individuals[row, "especie_fraction"])
  label <- num_especie_individuals[row, "num_ind_especie"]
  g <- g + annotate("text", x = x, y = y, label = label)
}
g <- g +
  ggtitle("80%-80% rule: Most contributing species and gears") +
  xlab("Species") +
  ylab("Species contribution to the sampling(%)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

g







