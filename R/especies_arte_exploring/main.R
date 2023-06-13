library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")
library("ggpubr")
library("tidyr")
source("config.R")
source("utils.R")
source("especies_arte_exploring/utils.R")
source("especies_arte_exploring/overall_report.R")
source("especies_arte_exploring/yearly_report.R")


if (!exists('db_data_tallas')) {
  clean_db_data_tallas <- read_csv(file.path(DATA_OUTPUT, 'clean_db_tallas.csv'), locale = locale(encoding = 'latin1'))
}

# (1) Build 80-80 rule dataframe from species-ARTE perspective
other_keyword <- 'Other'
esp_arte_report <- EspeciesArteReport$new(clean_db_data_tallas, 4)
esp_arte_report$generate_summary()
esp_arte_report$add_arte_nicknames()
db_data <- copy(esp_arte_report$overall_summary)
esp_arte_report$split_summary_by_threshold(80, 80, other_keyword)

mute <- esp_arte_report$summary_up_to_threshold %>%
  assert(not_na, colnames(.))
mute <- esp_arte_report$summary_from_threshold %>%
  assert(not_na, colnames(.))

yearly_esp_arte_report <- EspeciesArteYearReport$new(clean_db_data_tallas,
                                                     4,
                                                     esp_arte_report$summary_up_to_threshold,
                                                     other_keyword)
yearly_esp_arte_report$generate_summary()


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

years <- sort(unique((clean_db_data_tallas %>%
  mutate(year = as.numeric(format(largada_time, format = "%Y"))))$year))
plot_sheets <- generate_all_plots_spe_gear_dual_axis(yearly_esp_arte_report$summary,
                                                     years,
                                                     "Trisopterus luscus",
                                                     plot_context,
                                                     ceiling)

plots_to_pdf(list(plot_sheets[[1]]),
             "../data/sensitive/output/reports/t_luscus_main_ARTE_stats.pdf",
             paper_type,
             paper_height,
             paper_width)
# plots <- list()
# for (i in 1:16) {
#   data_to_plot <- yearly_esp_arte_report$summary[["Trisopterus luscus"]] %>%
#     filter(year == years[i])
#   mean_size <- mean(data_to_plot$mean_year_arte_talla)
#   mean_num_inds <- mean(data_to_plot$year_arte_abundance)
#   transf_factor <- get_nearest_base(mean_num_inds / mean_size)
#   plot_context.title <- years[i]
#   g <- generate_individual_plot_spe_gear_dual_axis(data_to_plot, plot_context, transf_factor, function(x) { ceiling(x) + 2 })
#   plots[[i]] <- g
# }
#
# outer_grid <-
#   ggarrange(
#     plotlist = plots,
#     ncol = 4,
#     nrow = 4,
#     common.legend = TRUE,
#     legend = "bottom"
#   )
# gg_plot <- annotate_figure(outer_grid,
#                            left = text_grob("Mean Length (cm)", rot = 90, vjust = 1),
#                            right = text_grob("Number of Individuals", rot = 90, vjust = 1),
#                            bottom = text_grob("Gears"))
#
# gg_plot

# # (2) Build plot 80-80 rule for especies and ARTE, respectively
# plot_context <- PlotContext$new()
# plot_context.title <- "80%-80% rule: Most contributing species \n and gears"
# plot_context.x_lab <- "Species"
# plot_context.y_lab <- "Species contribution to the sampling(%)"
# plot_context.legend_title <- 'Gears'
# plot_context.face_text_size <- 4
# plot_context.x_angle <- 45
# g_most_especies <- plot_especies_arte_barplot(esp_arte_report$summary_up_to_threshold,
#                                               plot_context,
#                                               vertical_adjusment_func = ceiling)
#
#
# # (3) Build plot 20-80 rule for especies and ARTE, respectively
# plot_context.title <- "20%-80% rule: Less contributing species and \n most contributed gears"
# plot_context.face_text_size <- 2.5
# plot_context.x_angle <- 90
# g_least_species <- plot_especies_arte_barplot(esp_arte_report$summary_from_threshold,
#                                               plot_context,
#                                               vertical_adjusment_func = function(x) round(x, 1) + 0.1)


#
# # (4) Plot findings and write report to disk
# outer_grid <-
#   ggarrange(
#     plotlist = list(
#       g_most_especies,
#       g_least_species
#     ),
#     ncol = 2,
#     nrow = 1
#   )
#
# plots_to_pdf(list(outer_grid),
#              "../data/sensitive/output/reports/species_gears_stack_barplot.pdf",
#              paper_type,
#              paper_height,
#              paper_width)
#
# write_csv(esp_arte_report$summary, "../data/sensitive/output/especies_arte_sampling.csv")