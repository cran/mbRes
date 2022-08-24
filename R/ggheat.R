#' Make Heatmap
#'
#' \code{ggheat} creates heatmaps of the Cliff's delta and S-value. This is not
#' meant to be called directly.
#'
#' @param dat a data frame with at least three columns.
#' @param hax a character, name of the column to be used as the horizontal axis.
#' @param vax a character, name of the column to be used as the vertical axis.
#' @param cell a character, name of the column to be used as the cells.
#' @param nm a character, name of the heatmap.
#' @param lim a numeric vector, limits of the color scale.
#' @param lo a character, color of the color scale low end.
#' @param hi a character, color of the color scale high end.
#' @param diverging a logical, whether to use diverging color gradient.
#' @param env an environment, to access outer scope variables.
#'
#' @return \code{ggheat} returns a ggplot object.
#'
#' @export
ggheat <-
  function(dat,
           hax,
           vax,
           cell,
           nm,
           lim,
           lo,
           hi,
           diverging = FALSE,
           env = parent.frame()) {
    ggplot(dat,
           aes(
             x = !!sym(hax),
             y = !!sym(vax),
             fill = !!sym(cell)
           )) +
      geom_tile(color = "white",
                size = 0.25) + {
                  if (env$display == TRUE)
                    geom_text(aes(label = round(!!sym(cell), 2)), colour = "black")
                } +
      scale_x_discrete(name = NULL,
                       expand = c(0, 0),
                       position = "top") +
      scale_y_discrete(name = NULL, limits = rev) + {
        if (diverging == TRUE)
          scale_fill_gradient2(
            low = lo,
            high = hi,
            limits = lim,
            name = NULL
          )
        else
          scale_fill_gradient(
            low = lo,
            high = hi,
            limits = lim,
            name = NULL
          )
      } +
      guides(
        fill = guide_colorbar(
          direction = "horizontal",
          label.position = "bottom",
          title.position = "top",
          ticks = FALSE,
          barwidth = unit(3.5, "in"),
          barheight = unit(0.2, "in")
        )
      ) +
      labs(title = nm) +
      theme_cowplot() +
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
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(1, "pt"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.title.align = 0.5,
        legend.title = element_text(size = 12, face = "bold")
      )
  }
