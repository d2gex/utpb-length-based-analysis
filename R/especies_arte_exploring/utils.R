library("ggplot2")

plot_especies_arte_barplot <- function(data, title, x_lab, y_lab, legend_title, x_angle, vertical_adjusment_func) {

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
  gg_plot <- add_text_top_every_bar(gg_plot,
                                    num_especie_individuals,
                                    x_col = 'ESPECIE',
                                    y_col = 'especie_fraction',
                                    label_col = "num_ind_especie",
                                    vertical_adjustment_function = vertical_adjusment_func)
  gg_plot <- gg_plot +
    ggtitle(title) +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_fill_discrete(name = legend_title) +
    theme(axis.text.x = element_text(angle = x_angle, vjust = 0.5))

  gg_plot
  return(gg_plot)

}
