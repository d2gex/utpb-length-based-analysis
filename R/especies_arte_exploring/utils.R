library("ggplot2")

plot_especies_arte_barplot <- function(data, plot_context, vertical_adjusment_func) {

  to_plot_df <- data %>%
    select(ESPECIE, arte_especie_absolute_fraction, ARTE, num_ind_especie) %>%
    arrange(ESPECIE, ARTE)

  num_especie_individuals <- data %>%
    select(ESPECIE, num_ind_especie, especie_fraction) %>%
    distinct()

  gg_plot <- to_plot_df %>% ggplot(
    aes(fill = reorder(ARTE, -arte_especie_absolute_fraction),
        x = reorder(ESPECIE, -num_ind_especie),
        y = arte_especie_absolute_fraction)) +
    geom_bar(position = "stack", stat = "identity")

  # Unfortunately each element on the x-axis may have different categories so
  # it is not possible to use geom_text for topping the bar with number on individuals
  gg_plot <- add_text_to_graph_position(gg_plot,
                                        num_especie_individuals,
                                        x_col = 'ESPECIE',
                                        y_col = 'especie_fraction',
                                        label_col = "num_ind_especie",
                                        label_text_size = plot_context$face_text_size,
                                        vertical_adjustment_function = vertical_adjusment_func)
  gg_plot <- gg_plot +
    ggtitle(plot_context$title) +
    xlab(plot_context$x_lab) +
    ylab(plot_context$y_lab) +
    scale_fill_discrete(name = plot_context$legend_title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = plot_context$x_angle, vjust = 0.5))

  return(gg_plot)

}

generate_single_year_plot_spe_gear_dual_axis <- function(year_data, plot_context, transf_factor, vertical_adjustment_func) {
        #' Generate an individual double-axis plot for a particular species, one single year and its main representative gears

  g <- ggplot(year_data, aes(x = reorder(ARTE, -mean_year_arte_talla))) +
    geom_bar(aes(y = mean_year_arte_talla, fill = ARTE), stat = "identity", size = .1) +
    geom_point(aes(y = year_arte_abundance / transf_factor))

  # do we have more than one gear per year so that we can draw a line?
  if (nrow(year_data) > 1) {
    g <- g + geom_line(aes(y = year_arte_abundance / transf_factor, group = 1), size = 0.5)
  }
  g <- g + scale_y_continuous(sec.axis = sec_axis(~. * transf_factor))

  g <- add_text_to_graph_position(
    g, year_data,
    'ARTE',
    'mean_year_arte_talla',
    'mean_year_arte_talla',
    plot_context$face_text_size,
    vertical_adjustment_func
  )

  g <- add_text_to_graph_position(
    g,
    year_data,
    'ARTE',
    year_data$year_arte_abundance / transf_factor,
    'year_arte_abundance',
    plot_context$face_text_size,
    vertical_adjustment_func
  )

  g <- g +
    theme_bw() +
    theme(legend.position = plot_context$legend_position,
          axis.text.x = element_text(angle = plot_context$x_angle, vjust = 0.5, size = plot_context$x_text_size),
          axis.text.y = element_text(size = plot_context$y_text_size),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = plot_context$title_size)) +
    ggtitle(plot_context$title)

  return(g)
}

generate_all_plots_spe_gear_dual_axis <- function(species_data, years, plot_context, vertical_adjustment_func) {
        #' Generate all-year double-axis plots for a particular species in the given data and its main representative gears
  # (1) Generate all plots per year
  plots <- list()
  species_years <- sort(unique(species_data$year))
  for (s_year in species_years) {
    year_data <- species_data %>%
      filter(year == s_year)
    mean_size <- mean(year_data$mean_year_arte_talla)
    mean_num_inds <- mean(year_data$year_arte_abundance)
    transf_factor <- get_nearest_base(mean_num_inds / mean_size)
    plot_context$title <- s_year
    g <- generate_single_year_plot_spe_gear_dual_axis(year_data,
                                                      plot_context, transf_factor, vertical_adjustment_func)
    plots[[as.character(s_year)]] <- g
  }

  # (2) Split plots into two sets (pdf sheets later on)
  if (length(species_years) != length(years)) {
    missing_years <- lapply(setdiff(years, species_years), as.character)
    empty_plots <- lapply(missing_years, function(x) {
      create_empty_plot(empty_message = 'Not available', title = x)
    })
    missing_plots <- setNames(empty_plots, unlist(unname(missing_years)))
    plots <- c(plots, missing_plots)
    plots <- plots[sort(names(plots))]
  }
  plot_groups <- list(list(subset = unname(plots[1:16]), ncol = 4, nrow = 4),
                      list(subset = unname(plots[17:length(years)]), ncol = 3, nrow = 3))


  plot_sheets <- list()
  for (i in seq_along(plot_groups)) {
    outer_grid <-
      ggarrange(
        plotlist = plot_groups[[i]]$subset,
        ncol = plot_groups[[i]]$ncol,
        nrow = plot_groups[[i]]$nrow,
        common.legend = TRUE,
        legend = "bottom"
      )
    gg_plot <- annotate_figure(outer_grid,
                               left = text_grob("Mean Length (cm)", rot = 90, vjust = 1),
                               right = text_grob("Number of Individuals", rot = 270, vjust = 1),
                               bottom = text_grob("Gears"))
    plot_sheets[[i]] <- gg_plot
  }
  return(plot_sheets)

}

generate_all_plots_all_spe_gear_dual_axis <- function(data, years, plot_context, vertical_adjustment_func) {
        #' Generate all-year double-axis plots for a all species in the given data and their main representative gears
  species <- names(data)
  species_plot <- list()
  for (ind_species in species) {
    species_data <- data[[ind_species]]
    s_plot_list <- generate_all_plots_spe_gear_dual_axis(species_data, years, plot_context, vertical_adjustment_func)
    species_plot[[ind_species]] <- s_plot_list
  }
  return(species_plot)
}