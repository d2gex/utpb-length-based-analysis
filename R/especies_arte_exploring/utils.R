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
                                        label_text_size = plot_context.face_text,
                                        vertical_adjustment_function = vertical_adjusment_func)
  gg_plot <- gg_plot +
    ggtitle(plot_context.title) +
    xlab(plot_context.x_lab) +
    ylab(plot_context.y_lab) +
    scale_fill_discrete(name = plot_context.legend_title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = plot_context.x_angle, vjust = 0.5))

  return(gg_plot)

}

generate_plot_spe_gear_dual_axis <- function(data, plot_context, transf_factor, vertical_adjustment_func) {

  g <- ggplot(data, aes(x = reorder(ARTE, -mean_year_arte_talla))) +
    geom_bar(aes(y = mean_year_arte_talla, fill = ARTE), stat = "identity", size = .1) +
    geom_point(aes(y = year_arte_abundance / transf_factor)) +
    geom_line(aes(y = year_arte_abundance / transf_factor, group = 1), size = 0.5) +

    scale_y_continuous(

      # # Features of the first axis
      # name = plot_context.y_lab,

      # Add a second axis and specify its features
      # sec.axis = sec_axis(~. * transf_factor, name = plot_context.second_y_lab)
      sec.axis = sec_axis(~. * transf_factor)
    )

  g <- add_text_to_graph_position(
    g, data,
    'ARTE',
    'mean_year_arte_talla',
    'mean_year_arte_talla',
    plot_context.face_text,
    vertical_adjustment_func
  )

  g <- add_text_to_graph_position(
    g,
    data,
    'ARTE',
    data$year_arte_abundance / transf_factor,
    'year_arte_abundance',
    plot_context.face_text,
    vertical_adjustment_func
  )


  g <- g +
    theme_bw() +
    theme(legend.position = plot_context.legend_position,
          axis.text.x = element_text(angle = plot_context.x_angle, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(plot_context.title)

  return(g)
}