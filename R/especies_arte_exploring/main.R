library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")
library("ggpubr")
source("config.R")
source("utils.R")
source("especies_arte_exploring/utils.R")
source("especies_arte_exploring/overall_report.R")


if (!exists('db_data_tallas')) {
  clean_db_data_tallas <- read_csv(file.path(DATA_OUTPUT, 'clean_db_tallas.csv'), locale = locale(encoding = 'latin1'))
}

# (1) Build 80-80 rule dataframe from species-ARTE perspective
esp_arte_report <- EspeciesArteReport$new(clean_db_data_tallas, 4)
esp_arte_report$generate_summary()
esp_arte_report$add_arte_nicknames()
db_data <- copy(esp_arte_report$overall_summary)
esp_arte_report$split_summary_by_threshold(80, 80, 'Other')

mute <- esp_arte_report$summary_up_to_threshold %>%
  assert(not_na, colnames(.))
mute <- esp_arte_report$summary_from_threshold %>%
  assert(not_na, colnames(.))


# (2) Build plot 80-80 rule for especies and ARTE, respectively
plot_context <- PlotContext$new()
plot_context.title <- "80%-80% rule: Most contributing species \n and gears"
plot_context.x_lab <- "Species"
plot_context.y_lab <- "Species contribution to the sampling(%)"
plot_context.legend_title <- 'Gears'
plot_context.face_text <- 4
plot_context.x_angle <- 45
g_most_especies <- plot_especies_arte_barplot(esp_arte_report$summary_up_to_threshold,
                                              plot_context,
                                              vertical_adjusment_func = ceiling)


# (3) Build plot 20-80 rule for especies and ARTE, respectively
plot_context.title <- "20%-80% rule: Less contributing species and \n most contributed gears"
plot_context.face_text <- 2.5
plot_context.x_angle <- 90
g_least_species <- plot_especies_arte_barplot(esp_arte_report$summary_from_threshold,
                                              plot_context,
                                              vertical_adjusment_func = function(x) round(x, 1) + 0.1)


# (4) Plot findings and write report to disk
outer_grid <-
  ggarrange(
    plotlist = list(
      g_most_especies,
      g_least_species
    ),
    ncol = 2,
    nrow = 1
  )

plots_to_pdf(list(outer_grid),
             "../data/sensitive/output/reports/species_gears_stack_barplot.pdf",
             paper_type,
             paper_height,
             paper_width)

write_csv(esp_arte_report$summary, "../data/sensitive/output/especies_arte_sampling.csv")