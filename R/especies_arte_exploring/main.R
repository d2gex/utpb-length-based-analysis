library("openxlsx")
library("ggplot2")
library("stringr")
library("readr")
library("ggpubr")
source("config.R")
source("utils.R")
source("especies_arte_exploring/utils.R")
source("especies_arte_exploring/overall_report.R")
source("especies_arte_exploring/yearly_report.R")


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

yearly_esp_arte_report <- EspeciesArteYearReport$new(clean_db_data_tallas, 4, esp_arte_report$summary_up_to_threshold)
yearly_esp_arte_report$generate_summary()
# dat_plot <- yearly_esp_arte_report$summary


# # (4) Generate yearly summaries for for each individual species
# specie_name <- 'Trisopterus luscus'
# other_keyword <- 'Other'
# specie_df <- clean_db_data_tallas %>%
#   filter(ESPECIE == specie_name) %>%
#   mutate(year = as.numeric(format(largada_time, format = "%Y"))) %>%
#   select(ESPECIE, TALLA, year, NUMINDIVS, ARTE) %>%
#   uncount(NUMINDIVS)
#
# # Potential NA in column NUMINDIVS should be gone by now
# specie_individuals <- sum((clean_db_data_tallas %>% filter(ESPECIE == specie_name))$NUMINDIVS)
# mute <- specie_df %>%
#   verify(nrow(.) == specie_individuals)
#
# # Get a single species' mains ARTE and num_individuals by ARTE
# specie_summary_df <- esp_arte_report$summary_up_to_threshold %>%
#   filter(ESPECIE == specie_name)
# specie_artes <- setdiff(unique(specie_summary_df$ARTE), other_keyword)
# specie_relevant_arte <- specie_df %>%
#   filter(ARTE %in% specie_artes)
#
# # Calculate yearly abundance per ARTE
# species_relevant_arte_abundance_df <- specie_relevant_arte %>%
#   filter(ARTE %in% specie_artes) %>%
#   group_by(year, ARTE) %>%
#   summarise(year_arte_abundance = n())
#
# # Ensure that abundances have been properly calculated
# mute <- species_relevant_arte_abundance_df %>%
#   summarise(year_abundance = sum(year_arte_abundance)) %>%
#   summarise(total_year_abundance = sum(year_abundance)) %>%
#   verify(total_year_abundance == nrow(specie_relevant_arte))
#
# # Calculate yearly TALLA per ARTE
# species_relevant_arte_meantalla_df <- specie_relevant_arte %>%
#   group_by(year, ARTE) %>%
#   summarise(mean_year_arte_talla = round(mean(TALLA), 2)) %>%
#   arrange(year, ARTE)
#
# data_plot <- merge(species_relevant_arte_abundance_df,
#                    species_relevant_arte_meantalla_df,
#                    by = c("year", "ARTE"),
#                    all = TRUE)
#
#
# coeff <- 100
# data_to_plot <- data_plot %>%
#   filter(year==1999)
#
# g <- ggplot(data_to_plot, aes(x = reorder(ARTE, -mean_year_arte_talla))) +
#   geom_bar(aes(y = mean_year_arte_talla, fill = ARTE), stat = "identity", size = .1) +
#   geom_point(aes(y=year_arte_abundance / coeff)) +
#   geom_line(aes(y = year_arte_abundance / coeff, group=1), size = 1) +
#
#   scale_y_continuous(
#
#     # Features of the first axis
#     name = "Mean Length",
#
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~. * coeff, name = "Number of individuals")
#   )
#
# g <- add_text_to_graph_position(
#   g, data_to_plot, 'ARTE', 'mean_year_arte_talla', 'mean_year_arte_talla', 6, ceiling
# )
#
# g <- add_text_to_graph_position(
#   g,
#   data_to_plot,
#   'ARTE',
#   data_to_plot$year_arte_abundance / coeff,
#   'year_arte_abundance',
#   6, ceiling
# )
#
# g <- g + theme_bw() +
#   xlab("Gears") +
#   ggtitle("Number of individuals vs Mean Length by year and gear in 1999")
#
# g


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