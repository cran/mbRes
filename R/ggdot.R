#' Make Dot Plot
#'
#' \code{ggdot} creates dot plot of the average of absolute Cliff’s delta. This
#' is not meant to be called directly.
#'
#' @param dat a data frame with at least two columns.
#' @param hax a character, name of the column to be used as the horizontal axis.
#' @param vax a character, name of the column to be used as the vertical axis.
#'
#' @return \code{ggdot} returns a ggplot object.
#'
#' @export
ggdot <- function(dat, hax, vax) {
  ggplot(dat, aes(x = !!sym(hax), y = fct_reorder(!!sym(vax),!!sym(hax)))) +
    geom_point(
      pch = 21,
      fill = "#E69F00",
      color = "#000000",
      size = 5
    ) +
    geom_text(
      aes(label = round(!!sym(hax), 2)),
      nudge_x = -0.075,
      nudge_y = 0.25,
      color = "#000000"
    ) +
    scale_x_continuous(name = NULL,
                       limits = c(0, 1)) +
    scale_y_discrete(name = NULL) +
    labs(title = "Average |Cliff's delta|") +
    theme_minimal_grid() +
    theme(plot.title = element_text(hjust = 0.5))
}
