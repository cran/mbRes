#' Compare RSI, IBR, and Cliff's delta
#'
#' \code{compare} calculates RSI assigned values, IBR translated scores, and
#' Cliff's delta in the hypothetical case study.
#'
#' @param sam a data frame, the first output of \code{\link{simul}}.
#' @param sam_mean a data frame, the third output of \code{\link{simul}}.
#'
#' @references Blaise, C., Gagné, F., Pellerin, J., Hansen, P.-D., & Trottier,
#'   S. (2002). Molluscan shellfish biomarker study of the Quebec, Canada,
#'   Saguenay Fjord with the soft-shell clam, Mya arenaria. Environmental
#'   Toxicology, 17(3), 170–186. \doi{10.1002/tox.10048}.  \cr \cr Beliaeff, B.,
#'   & Burgeot, T. (2002). Integrated biomarker response: A useful tool for
#'   ecological risk assessment. Environmental Toxicology and Chemistry, 21(6),
#'   1316–1322. \doi{10.1002/etc.5620210629}.
#'
#' @return \code{compare} returns a list of length 5: \item{\code{blaise}}{RSI
#'   assigned values and final RSI.} \item{\code{beliaeff}}{IBR translated
#'   scores and final IBR.} \item{\code{pham}}{Cliff's delta and the average of
#'   absolute Cliff’s delta.} \item{\code{fig1}}{ggplot object of comparisions
#'   among RSI assigned values, IBR translated scores, and Cliff's delta.}
#'   \item{\code{fig2}}{ggplot object of comparision among RSI, IBR, and the
#'   average of absolute Cliff’s delta.}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' setting <- setpop()
#' temp <- simul(setting$pop_mean)
#' compare(temp$sam, temp$sam_mean)
#' } #might take more than 5s in some machines
#'
#' @export
compare <- function(sam, sam_mean){
  Index <- Biomarker <- Value <- Bmk1 <- Bmk5 <- Site <- avg <- NULL #no visible binding for global variable

  fivecolor <- c("#E69F00",     "#56B4E9" ,    "#009E73"  ,  "#D55E00"   , "#CC79A7")

  blaise <- blaise2002(sam, sam_mean)
  beliaeff <- beliaeff2002(sam_mean)
  pham_full <- mbr(sam)

  p1 <- blaise %>% select(-Index) %>% gather(Biomarker, Value, Bmk1:Bmk5) %>%
    ggplot(aes(x = Site,
               y = Value,
               group = 1)) +
    geom_line(size = 1.5, color = rep(
      fivecolor,
      each = 5
    )) + geom_point(size = 5, shape = 21, color = "white", fill = rep(
      fivecolor,
      each = 5
    )) +
    facet_wrap(~ Biomarker, nrow = 1) +
    ylab("RSI assigned value")+
    xlab(NULL)+
    theme_minimal_grid(font_size  = 14, rel_small = 1) +
    theme(
      axis.text.x=element_blank()
    )

  p2 <- beliaeff %>% select(-Index) %>% gather(Biomarker, Value, Bmk1:Bmk5) %>%
    ggplot(aes(x = Site,
               y = Value,
               group = 1)) +
    geom_line(size = 1.5, color = rep(
      fivecolor,
      each = 5
    )) + geom_point(size = 5, shape = 21, color = "white", fill = rep(
      fivecolor,
      each = 5
    )) +
    facet_wrap(~ Biomarker, nrow = 1) +
    ylab("IBR translated score")+
    xlab(NULL)+
    theme_minimal_grid(font_size  = 14, rel_small = 1) +
    theme(strip.text.x = element_blank(),
          axis.text.x=element_blank()
    )

  pham <- pham_full$idx %>% add_row(
    Site = "S0",
    Bmk1 = 0,
    Bmk2 = 0,
    Bmk3 = 0,
    Bmk4 = 0,
    Bmk5 =0,
    avg = 0,
    .before = 1
  )
  p3 <-  pham %>% select(-avg) %>% gather(Biomarker, Value, Bmk1:Bmk5) %>%
    ggplot(aes(x = Site,
               y = Value,
               group = 1)) +
    geom_hline(yintercept = 0, linetype="dashed", size = 1)+
    geom_line(size = 1.5, color = rep(
      fivecolor,
      each = 5
    )) + geom_point(size = 5, shape = 21, color = "white", fill = rep(
      fivecolor,
      each = 5
    )) +
    facet_wrap(~ Biomarker, nrow = 1) +
    ylab("Cliff's delta")+
    theme_minimal_grid(font_size  = 14, rel_small = 1) +
    theme(strip.text.x = element_blank())

  fig1 <- plot_grid(p1, p2, p3, ncol = 1, align ="hv", labels="auto")

  p4 <- blaise %>%
    ggplot(aes(x = Site,
               y = Index,
               group = 1)) +
    geom_line(size = 1.5, color = "#0072B2") + geom_point(size = 5, shape = 22, color = "white", fill = "#0072B2") +
    labs(x = NULL, y = NULL, title = "RSI") +
    theme_cowplot(font_size  = 14, rel_small = 1) +
    theme(plot.title = element_text(hjust = 0.5))

  p5 <- beliaeff %>%
    ggplot(aes(x = Site,
               y = Index,
               group = 1)) +
    geom_line(size = 1.5, color = "#0072B2") + geom_point(size = 5, shape = 23, color = "white", fill = "#0072B2") +
    labs(y = NULL, title = "IBR") +
    theme_cowplot(font_size  = 14, rel_small = 1) +
    theme(plot.title = element_text(hjust = 0.5))

  p6 <- pham %>%
    ggplot(aes(x = Site,
               y = avg,
               group = 1)) +
    geom_line(size = 1.5, color = "#0072B2") + geom_point(size = 5, shape = 24, color = "white", fill = "#0072B2") +
    labs(x = NULL, y = NULL, title = "Average |Cliff's delta|") +
    theme_cowplot(font_size  = 14, rel_small = 1) +
    theme(plot.title = element_text(hjust = 0.5))

  fig2 <- plot_grid(p4, p5, p6, nrow = 1, align ="hv")

  list(
    blaise = blaise,
    beliaeff = beliaeff,
    pham = pham,
    fig1 = fig1,
    fig2 = fig2
  )

}
