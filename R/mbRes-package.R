#' mbRes: Integrating Multiple Biomarker Responses in Aquatic Organisms using
#' Effect Size, Statistical Uncertainty, and Ecological Relevance
#'
#' Compute and visualize the ps-index, a new integrated index for multiple
#' biomarker responses, as described in Pham & Sokolova (2021, unpublished).
#'
#' @author Duy Nghia Pham & Inna M. Sokolova
#'
#' @docType package
#'
#' @name mbRes-package
#'
#' @section Guidelines: ps-index is a new integrated index for multiple
#'   biomarker responses. \code{\link{mbr}} is the main function to compute and
#'   visualize the ps-index. \code{\link{sokolova2021}} is provided as a sample
#'   dataset. \code{\link{compare}} simulates a hypothetical dataset and compare
#'   the results of ps-index and two other integrated indices published earlier.
#'   The others are helper functions and are not meant to be called directly by
#'   users.
#'
#' @section Copyright: mbRes: Integrating Multiple Biomarker Responses in
#'   Aquatic Organisms using Effect Size, Statistical Uncertainty, and
#'   Ecological Relevance. Copyright (C) 2021  Duy Nghia Pham & Inna M. Sokolova
#'   \cr \cr mbRes is free software: you can redistribute it and/or modify it
#'   under the terms of the GNU General Public License as published by the Free
#'   Software Foundation, either version 3 of the License, or (at your option)
#'   any later version. \cr \cr mbRes is distributed in the hope that it will be
#'   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
#'   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
#'   Public License for more details. \cr \cr You should have received a copy of
#'   the GNU General Public License along with mbRes.  If not, see
#'   \url{https://www.gnu.org/licenses/}.
#'
#' @importFrom stats na.omit pnorm qnorm rnorm weighted.mean quantile sd
#'   wilcox.test
#' @importFrom magrittr %>% %$%
#' @importFrom dplyr case_when select transmute mutate group_by summarise
#'   rowwise dense_rank percent_rank min_rank ntile cume_dist rename
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom data.table rbindlist
#' @importFrom cowplot theme_cowplot theme_minimal_hgrid theme_minimal_grid
#'   plot_grid get_legend draw_label panel_border
#' @importFrom grid unit
#' @importFrom forcats fct_reorder
#' @importFrom tidyr gather spread
#' @importFrom tibble add_row
#' @importFrom ggforce geom_sina
#'@importFrom utils data
#' @import ggplot2
NULL
