#' Plot the top N sensitivity values
#'
#' @param sensitivity_plot_data Formatted sensitivity results
#' @param y_var Response variable for which to plot sensitivity (character)
#' @param metric Sensitivity metric to plot, as _symbol_ (e.g. `elasticiity`,
#'   `pvar`)
#' @param n Show this many values per panel
#' @return `ggplot` object of plot
#' @author Alexey Shiklomanov
#' @export
top_n_sensitivity_plot <- function(sensitivity_plot_data, y_var, metric,
                                   n = 10, scales = "free_y") {
  top_x <- sensitivity_plot_data %>%
    group_by(model, yvar) %>%
    top_n(n, abs({{metric}})) %>%
    ungroup() %>%
    arrange(yvar, model, abs({{metric}})) %>%
    mutate(i = row_number())

  top_x %>%
    filter(yvar == y_var) %>%
    ggplot() +
    aes(x = i, y = {{metric}}, color = shortname) +
    geom_segment(aes(xend = i, yend = 0)) +
    geom_point() +
    facet_wrap(vars(model), scales = scales, drop = TRUE) +
    coord_flip() +
    scale_x_continuous(
      breaks = top_x$i,
      labels = top_x$xvar
    )
}
