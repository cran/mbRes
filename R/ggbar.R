#' Make Bar Plot
#'
#' \code{ggbar} creates bar plot of the ecological relevance. This is not meant
#' to be called directly.
#'
#' @param dat a data frame with at least two columns.
#' @param hax a character, name of the column to be used as the horizontal axis.
#' @param vax a character, name of the column to be used as the vertical axis.
#' @param sub a numeric, mean ecological relevance.
#' @param env an environment, to access outer scope variables.
#'
#' @return \code{ggbar} returns a ggplot object.
#'
#' @export
ggbar <- function(dat, hax, vax, sub, env = parent.frame()) {
  ggplot(dat, aes(x = !!sym(hax), y = !!sym(vax))) +
    geom_col(fill = "#009E73", alpha = 0.9) +
    scale_y_reverse(name = NULL) +
    scale_x_discrete(name = NULL,
                     expand = c(0, 0),
                     position = "top") +
    labs(title = "Ecological relevance") +
    geom_hline(yintercept=sub, linetype="dashed") +
    theme_minimal_hgrid() +
    {
      if (env$rotate == TRUE)
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0,
          hjust = 0
        ))
    } +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}
