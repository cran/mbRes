#'mbRes: Exploration of Multiple Biomarker Responses using Effect Size
#'
#'Summarize multiple biomarker responses of aquatic organisms to contaminants
#'using Cliff’s delta, as described in Pham & Sokolova (2023)
#'\doi{10.1002/ieam.4676}.
#'
#'@author Duy Nghia Pham & Inna M. Sokolova
#'
#'@docType package
#'
#'@name mbRes-package
#'
#'@section Guidelines: \code{\link{mbr}} and \code{\link{visual}} are the main
#'  functions to compute and visualize Cliff’s delta and S-value which are
#'  results of \code{\link{cliff}} and \code{\link{resampling}}.
#'  \code{\link{setpop}}, \code{\link{simul}}, and \code{\link{plotsam}}
#'  simulate and visualize a hypothetical dataset. \code{\link{compare}}
#'  compares the results of Cliff’s delta and two other integrated indices
#'  published earlier (i.e., RSI and IBR, see \code{\link{blaise2002}} and
#'  \code{\link{beliaeff2002}}). The others (\code{\link{ggheat}} and
#'  \code{\link{ggdot}}) are helper functions and are not meant to be called
#'  directly by users.
#'
#'@section Updates: \code{\link{mbr.cliff}} and \code{\link{mbr.glass}} simply
#'  compute and visualize Cliff’s delta and Glass's delta.
#'
#'@section Copyright: mbRes: Exploration of Multiple Biomarker Responses using
#'  Effect Size. \cr Copyright (C) 2021-2023 Duy Nghia Pham & Inna M. Sokolova
#'  \cr \cr mbRes is free software: you can redistribute it and/or modify it
#'  under the terms of the GNU General Public License as published by the Free
#'  Software Foundation, either version 3 of the License, or (at your option)
#'  any later version. \cr \cr mbRes is distributed in the hope that it will be
#'  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
#'  Public License for more details. \cr \cr You should have received a copy of
#'  the GNU General Public License along with mbRes.  If not, see
#'  \url{https://www.gnu.org/licenses/}.
#'
#'@importFrom stats na.omit rnorm sd wilcox.test
#'@importFrom magrittr %>%
#'@importFrom dplyr full_join group_by mutate rename rename_with rowwise select
#'  summarise transmute
#'@importFrom tibble add_row
#'@importFrom forcats fct_reorder
#'@importFrom tidyr gather spread
#'@importFrom purrr map reduce
#'@importFrom data.table rbindlist
#'@importFrom scales squish
#'@import ggplot2 cowplot
#'
NULL
