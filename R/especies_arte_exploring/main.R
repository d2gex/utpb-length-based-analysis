library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")
library("ggpubr")
library("tidyr")
library("logger")
source("config.R")
source("utils.R")
source("especies_arte_exploring/utils.R")
source("especies_arte_exploring/overall_report.R")
source("especies_arte_exploring/yearly_report.R")

log_info("---------Generating plots that show most representative gears for all species ---------------")
if (!exists('db_data_tallas')) {
  clean_db_data_tallas <- read_csv(file.path(DATA_OUTPUT, 'clean_db_tallas.csv'), locale = locale(encoding = 'latin1'))
}

log_info("--> (2) Generate overall species-main-gears plots for most contributing species")
# (1) Build 80-80 rule dataframe from species-ARTE perspective
other_keyword <- 'Other'
years <- sort(unique((clean_db_data_tallas %>%
  mutate(year = as.numeric(format(largada_time, format = "%Y"))))$year))

overall_esp_arte_report <- EspeciesArteReport$new(clean_db_data_tallas, 4)
overall_esp_arte_report$generate_summary()
overall_esp_arte_report$add_arte_nicknames()
db_data <- copy(overall_esp_arte_report$overall_summary)
overall_esp_arte_report$split_summary_by_threshold(80, 80, other_keyword)

mute <- overall_esp_arte_report$summary_up_to_threshold %>%
  assert(not_na, colnames(.))
mute <- overall_esp_arte_report$summary_from_threshold %>%
  assert(not_na, colnames(.))

plot_context <- PlotContext$new()
plot_context.title <- "80%-80% rule: Most contributing species \n and gears"
plot_context.x_lab <- "Species"
plot_context.y_lab <- "Species contribution to the sampling(%)"
plot_context.legend_title <- 'Gears'
plot_context.face_text_size <- 4
plot_context.x_angle <- 45
g_most_especies <- plot_especies_arte_barplot(overall_esp_arte_report$summary_up_to_threshold,
                                              plot_context,
                                              vertical_adjusment_func = ceiling)

log_info("--> (3) Generate overall species-main-gears plots for less contributing species")
# (3) Build plot 20-80 rule for especies and ARTE, respectively
plot_context.title <- "20%-80% rule: Less contributing species and \n most contributed gears"
plot_context.face_text_size <- 2.5
plot_context.x_angle <- 90
g_least_species <- plot_especies_arte_barplot(overall_esp_arte_report$summary_from_threshold,
                                              plot_context,
                                              vertical_adjusment_func = function(x) round(x, 1) + 0.1)

log_info("--> (4) Generate individual species-main-gears plots for most contributing species")
plot_context <- PlotContext$new()
plot_context$x_lab <- "Gears"
plot_context$y_lab <- "Mean Length (cms)"
plot_context$second_y_lab <- "Number of individuals"
plot_context$legend_title <- 'Gears'
plot_context$title_size <- 8
plot_context$face_text_size <- 2
plot_context$x_text_size <- 5
plot_context$y_text_size <- 5
plot_context$x_angle <- 45
plot_context$legend_position <- "none"

yearly_esp_arte_report_80 <- EspeciesArteYearReport$new(clean_db_data_tallas,
                                                        4,
                                                        overall_esp_arte_report$summary_up_to_threshold,
                                                        other_keyword)
yearly_esp_arte_report_80$generate_summary()
up_to_80_species_plots <- generate_all_plots_all_spe_gear_dual_axis(yearly_esp_arte_report_80$summary,
                                                                    years,
                                                                    plot_context,
                                                                    ceiling)

log_info("--> (5) Generate individual species-main-gears plots for less contributing species")
yearly_esp_arte_report_20 <- EspeciesArteYearReport$new(clean_db_data_tallas,
                                                        4,
                                                        overall_esp_arte_report$summary_from_threshold,
                                                        other_keyword)
yearly_esp_arte_report_20$generate_summary()
from_80_species_plots <- generate_all_plots_all_spe_gear_dual_axis(yearly_esp_arte_report_20$summary,
                                                                   years,
                                                                   plot_context,
                                                                   ceiling)

log_info("--> (6) Save overall species-main-gears reports")
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
             "../data/sensitive/output/reports/overall_species_main_arte/overall_species_main_summary.pdf",
             paper_type,
             paper_height,
             paper_width)


log_info("--> (7) Save individual species-main-gears reports")
for (ind_species_plots in names(up_to_80_species_plots)) {
  out_path <- paste0("../data/sensitive/output/reports/individual_species_main_arte/up_to_80/", gsub(" ", "_", ind_species_plots), ".pdf")
  plots_to_pdf(up_to_80_species_plots[[ind_species_plots]],
               out_path,
               paper_type,
               paper_height,
               paper_width)

}

for (ind_species_plots in names(from_80_species_plots)) {
  out_path <- paste0("../data/sensitive/output/reports/individual_species_main_arte/from_80/", gsub(" ", "_", ind_species_plots), ".pdf")
  plots_to_pdf(from_80_species_plots[[ind_species_plots]],
               out_path,
               paper_type,
               paper_height,
               paper_width)

}

log_info("--> (8) Write down overall and main individual csvs")
write_csv(overall_esp_arte_report$summary_up_to_threshold,
          "../data/sensitive/output/reports/overall_species_main_arte/overall_80.csv")
write_csv(overall_esp_arte_report$summary_from_threshold,
          "../data/sensitive/output/reports/overall_species_main_arte/overall_20.csv")

for (ind_species in names(yearly_esp_arte_report_80$summary)) {
  out_path <- paste0("../data/sensitive/output/reports/individual_species_main_arte/up_to_80/", gsub(" ", "_", ind_species), ".csv")
  write_csv(yearly_esp_arte_report_80$summary[[ind_species]],
            out_path)

}
for (ind_species in names(yearly_esp_arte_report_20$summary)) {
  out_path <- paste0("../data/sensitive/output/reports/individual_species_main_arte/from_80/", gsub(" ", "_", ind_species), ".csv")
  write_csv(yearly_esp_arte_report_20$summary[[ind_species]],
            out_path)

}

