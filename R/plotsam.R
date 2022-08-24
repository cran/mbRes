#' Visualize Hypothetical Samples
#'
#' \code{plotsam} plots the sample dataset of biomarker responses. This is used
#' for the hypothetical case study.
#'
#' @param pop_mean_long a data frame, the second output of \code{\link{setpop}}.
#' @param pop_profile a data frame, the third output of \code{\link{setpop}}.
#' @param sam_long a data frame, the second output of \code{\link{simul}}.
#'
#' @return \code{plotsam} returns a ggplot object.
#'
#' @examples
#' set.seed(1)
#' setting <- setpop()
#' temp <- simul(setting$pop_mean)
#' plotsam(setting$pop_mean_long, setting$pop_profile, temp$sam_long)
#'
#' @export
plotsam <- function(pop_mean_long, pop_profile, sam_long) {

  Site <- Response <- Mean_Response <- Profile <- NULL #no visible binding for global variable

  fivecolor <- c("#E69F00",     "#56B4E9" ,    "#009E73"  ,  "#D55E00"   , "#CC79A7")
  sam_fig <-
    sam_long %>% ggplot(aes(x = Site, y = Response)) +
    geom_boxplot(fill = "grey90") +
    stat_summary(
      fun = mean,
      geom = "line",
      aes(group = 1),
      size = 1.5,
      color = rep(
        fivecolor,
        each = 5
      )
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 5,
      color = "white",
      fill = rep(
        fivecolor,
        each = 5
      )
    ) +

    scale_y_continuous(breaks = seq(-3, 5, 1)) +
    geom_text(data = pop_mean_long,
              aes(x = , y = -4, label = Mean_Response),
              fontface = "italic") +
    geom_text(
      data = pop_profile,
      aes(x = 3, y = 6, label = Profile),
      fontface = "bold",
      color = fivecolor, size = 5
    ) +
    facet_wrap(~ Biomarker, nrow = 1) +
    theme_minimal_hgrid(font_size  = 14, rel_small = 1) +
    panel_border(size = 1.5)

  sam_fig
}
