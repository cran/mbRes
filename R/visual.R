#' Visualize Cliff's delta and S-value
#'
#' \code{visual} plots Cliff's delta and S-value for multiple groups and
#' multiple biomarkers.
#'
#' @param rs a list, output of \code{\link{mbr}}.
#' @param rotate a logical, whether to rotate the biomarker labels in figures.
#' @param display a logical, whether to display cell values in heatmaps.
#'
#' @return \code{visual} returns a list of ggplot objects:
#'   \item{\code{fig.delta}}{heatmap of Cliff's delta.}
#'   \item{\code{fig.sval}}{heatmap of S-value.} \item{\code{fig.avg}}{dot plot
#'   of the average of absolute Cliff’s delta.} \item{\code{mbr_fig}}{combined
#'   heatmaps of Cliff's delta and S-value.}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' setting <- setpop()
#' temp <- simul(setting$pop_mean)
#' mbr_result <- mbr(temp$sam)
#' visual(mbr_result)
#' } #might take more than 5s in some machines
#'
#' @export
visual <- function(rs,
                   rotate = FALSE,
                   display = TRUE) {
  fig.delta <-
    ggheat(
      rs$es,
      "biomarker",
      "test_site",
      "delta",
      "Cliff's delta",
      c(-1, 1),
      "#0072B2",
      "#D55E00",
      diverging = TRUE
    )

  fig.sval <-
    ggheat(rs$es,
           "biomarker",
           "test_site",
           "sval",
           "Surprisal",
           c(0, NA),
           "white",
           "#CC79A7")

  fig.avg <- ggdot(rs$idx, "avg", colnames(rs$idx)[1])

  mbr_fig <-
    plot_grid(
      fig.delta,
      fig.sval,
      ncol = 1,
      align = "hv",
      labels = "auto"
    )

  list(
    fig.delta = fig.delta,
    fig.sval = fig.sval,
    fig.avg = fig.avg,
    mbr_fig = mbr_fig
  )
}
