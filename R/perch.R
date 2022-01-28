#' Empirical Case Study Analysis
#'
#' \code{perch} analyses the biomarker results of Hansson et al. (2014)
#' <doi:10.1007/s00244-013-9974-5>.
#'
#' @return \code{perch} returns a list of length 3: \item{\code{tab}}{a list of
#'   length 2 \describe{ \item{\code{hansson2014}}{biomarker data reported by
#'   Hansson et al. (2014)} \item{\code{percheco}}{assigned ecological relevance
#'   of biomarkers}
#'   }} \item{\code{fig}}{a list of 5 ggplot objects \describe{
#'   \item{\code{SG}}{length-corrected somatic growth} \item{\code{SCI}}{somatic
#'   condition index} \item{\code{GSI}}{time-corrected gonadosomatic index}
#'   \item{\code{LSI}}{time-corrected liver somatic index}
#'   \item{\code{EROD}}{liver ethoxyresorufin-O-deethylase activity} } }
#'   \item{\code{est}}{full results of our estimation method given by
#'   \code{mbr}}
#'
#' @references Hansson, T., Hansen, W., Tjärnlund, U., Balk, L., & Bengtsson,
#'   B.-E. (2014). Biomarker Investigations in Adult Female Perch (Perca
#'   fluviatilis) From Industrialised Areas in Northern Sweden in 2003. Archives
#'   of Environmental Contamination and Toxicology, 66(2), 237–247.
#'   \doi{10.1007/s00244-013-9974-5}.
#'
#' @export
perch <- function(){
  Site <- NULL
  col <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  set.seed(1)
  SG <- hansson2014 %>%
    ggplot(aes(x = Site, y = SG)) +
    geom_sina(aes(color = Site), size = 2) +
    scale_colour_manual(values = col) +
    theme_cowplot() +
    theme(plot.margin = margin(3, 7, 3, 1.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "SG", x = "", y = "g/year")

  SCI <- hansson2014 %>%
    ggplot(aes(x = Site, y = SCI)) +
    geom_sina(aes(color = Site), size = 2) +
    scale_colour_manual(values = col) +
    theme_cowplot() +
    theme(plot.margin = margin(3, 7, 3, 1.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "SCI", x = "", y = expression(g/cm^3 ~ paste("*") ~ 100))

  GSI <- hansson2014 %>%
    ggplot(aes(x = Site, y = GSI)) +
    geom_sina(aes(color = Site), size = 2) +
    scale_colour_manual(values = col) +
    theme_cowplot() +
    theme(plot.margin = margin(3, 7, 3, 1.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "GSI", x = "", y = "%")

  LSI <- hansson2014 %>%
    ggplot(aes(x = Site, y = LSI)) +
    geom_sina(aes(color = Site), size = 2) +
    scale_colour_manual(values = col) +
    theme_cowplot() +
    theme(plot.margin = margin(3, 7, 3, 1.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "LSI", x = "", y = "%")

  EROD <- hansson2014 %>%
    ggplot(aes(x = Site, y = EROD)) +
    geom_sina(aes(color = Site), size = 2) +
    scale_colour_manual(values = col) +
    theme_cowplot() +
    theme(plot.margin = margin(3, 7, 3, 1.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "EROD", x = "", y = "pmol/min/g")

  estimation <- mbr(hansson2014, percheco)

  tab <- list(hansson2014=hansson2014, percheco=percheco)
  fig <- list(SG=SG, SCI=SCI, GSI=GSI, LSI=LSI, EROD=EROD)

  list(tab = tab, fig = fig, est = estimation)


}
