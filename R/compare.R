#' Hypothetical Case Study Analysis
#'
#' \code{compare} generates a hypothetical dataset of biomarker responses and
#' analyzes the dataset using three different strategies.
#'
#' @return \code{compare} returns a list of length 3: \item{\code{tab}}{a list
#'   of length 9 \describe{ \item{\code{pop}}{the true population means of
#'   normal distributions} \item{\code{pop_long}}{the true population means in
#'   long format} \item{\code{sam}}{the sample dataset of biomarker responses}
#'   \item{\code{sam_long}}{the sample dataset in long format}
#'   \item{\code{m}}{mean biomarker responses by exposure group}
#'   \item{\code{blaise2002}}{integrated index from an ordinalization method}
#'   \item{\code{beliaeff2002}}{integrated index from a standardization method}
#'   \item{\code{eco}}{assigned ecological relevance of biomarkers}
#'   \item{\code{pham2021}}{ps-index from our estimation method} }}
#'   \item{\code{fig}}{a list of 2 ggplot objects \describe{
#'   \item{\code{fig_sam}}{hypothetical dataset}
#'   \item{\code{fig_compare}}{result comparision of three methods} } }
#'   \item{\code{est}}{full results of our estimation method given by
#'   \code{mbr}}
#' @export
compare <- function() {
  Biomarker <-
    Bmk1 <-
    Bmk2 <-
    Bmk3 <- Exposure <- Index <- Response <- Value <- Variable <-
    biomarker <-
    delta.abs <-
    mean.Bmk1 <-
    mean.Bmk2 <-
    mean.Bmk3 <- 'ps-index' <- treatment <- weight <- zscore <-
    NULL #no visible binding for global variable

  #simulation
  pop <- data.frame(
    Exposure = c("D0", "D1", "D2", "D3", "D4"),
    Bmk1 = c(0, 0.5, 1, 0.5, 0),
    Bmk2 = c(0, 0.5, 1, 1.5, 2),
    Bmk3 = c(0, -0.5, -1, -1.5, -2)
  )
  pop_long <- pop %>% gather(Biomarker, Response, Bmk1:Bmk3)

  simul <- function(x)
    rnorm(n = 30, mean = x, sd = 1)
  set.seed(1)
  sam <-
    data.frame(
      Exposure = rep(c("D0", "D1", "D2", "D3", "D4"), each = 30),
      Bmk1 = c(simul(0), simul(0.5), simul(1), simul(0.5), simul(0)),
      Bmk2 = c(simul(0), simul(0.5), simul(1), simul(1.5), simul(2)),
      Bmk3 = c(simul(0), simul(-0.5), simul(-1), simul(-1.5), simul(-2))
    )

  sam_long <- sam %>% gather(Biomarker, Response, Bmk1:Bmk3)

  fig_sam <-
    sam_long %>% ggplot(aes(x = Exposure, y = Response)) +
    geom_boxplot(fill = "grey90") +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 3,
      color = "#D55E00",
      fill = "#D55E0080"
    ) +
    scale_y_continuous(breaks = c(-4, -2, 0, 2, 4)) +
    geom_text(data = pop_long,
              aes(x = Exposure, y = -5.5, label = Response),
              fontface = "italic") +
    facet_wrap(~ Biomarker) +
    theme_minimal_hgrid(font_size  = 14, rel_small = 1) +
    panel_border(size = 1.5)
  #mean by exposure
  m <- sam %>% group_by(Exposure) %>% summarise(
    mean.Bmk1 = mean(Bmk1),
    mean.Bmk2 = mean(Bmk2),
    mean.Bmk3 = mean(Bmk3)
  )
  #blaise2002
  ##Bmk1
  sorting <- m$Exposure[order(m$mean.Bmk1)]
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk1[sam$Exposure == sorting[i]],
                               sam$Bmk1[sam$Exposure == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df1 <- data.frame(sorting, Bmk1 = rank)
  ##Bmk2
  sorting <- m$Exposure[order(m$mean.Bmk2)]
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk2[sam$Exposure == sorting[i]],
                               sam$Bmk2[sam$Exposure == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df2 <- data.frame(sorting, Bmk2 = rank)
  ##Bmk3
  sorting <- m$Exposure[order(-m$mean.Bmk3)]
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk3[sam$Exposure == sorting[i]],
                               sam$Bmk3[sam$Exposure == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df3 <- data.frame(sorting, Bmk3 = rank)
  ##Index
  blaise2002 <-
    merge(merge(df1, df2), df3) %>% rowwise %>% mutate(Index = sum(Bmk1, Bmk2, Bmk3)) %>% rename(Exposure = sorting)

  #beliaeff2002
  ##Bmk1
  pool <- m %>% summarise(mean = mean(mean.Bmk1),
                          sd = sd(mean.Bmk1))
  temp <- m %>% select(Exposure, mean.Bmk1) %>% rowwise %>%
    mutate(zscore = (mean.Bmk1 - pool$mean) / pool$sd)
  dat1 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))
  ##Bmk2
  pool <- m %>% summarise(mean = mean(mean.Bmk2),
                          sd = sd(mean.Bmk2))
  temp <- m %>% select(Exposure, mean.Bmk2) %>% rowwise %>%
    mutate(zscore = (mean.Bmk2 - pool$mean) / pool$sd)
  dat2 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))
  ##Bmk3
  pool <- m %>% summarise(mean = mean(mean.Bmk3),
                          sd = sd(mean.Bmk3))
  temp <- m %>% select(Exposure, mean.Bmk3) %>% rowwise %>%
    mutate(zscore = -(mean.Bmk3 - pool$mean) / pool$sd)
  dat3 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))
  ##Index
  beliaeff2002 <-
    data.frame(
      Exposure = dat1$Exposure,
      Bmk1 = dat1$trans.score,
      Bmk2 = dat2$trans.score,
      Bmk3 = dat3$trans.score
    ) %>%
    mutate(
      Index = 0.5 * Bmk1 * Bmk2 * sin(120 * pi / 180) +
        0.5 * Bmk2 * Bmk3 * sin(120 * pi / 180) +
        0.5 * Bmk3 * Bmk1 * sin(120 * pi / 180)
    )
  #pham2021
  eco <- data.frame(bmk = c("Bmk1", "Bmk2", "Bmk3"),
                    eco = c(1, 3, 3))
  estimation <- mbr(sam, eco)
  dfa <-
    estimation$es %>% select(treatment, biomarker, delta.abs) %>% spread(biomarker, delta.abs)

  dfb <- estimation$es %>% group_by(treatment) %>%
    summarise(
      'Unweighted index' = mean(delta.abs),
      'ps-index' = weighted.mean(delta.abs, weight)
    )
  pham2021 <- merge(dfa, dfb)  %>% add_row(
    treatment = "D0",
    Bmk1 = NA,
    Bmk2 = NA,
    Bmk3 = NA,
    'Unweighted index' = NA,
    `ps-index` = NA,
    .before = 1
  ) %>% rename(Exposure = treatment)

  #visualize
  plot1 <- blaise2002 %>% gather(Variable, Value, Bmk1:Index) %>%
    ggplot(
      aes(
        x = Exposure,
        y = Value,
        group = Variable,
        color = Variable,
        fill = Variable,
        shape = Variable,
        linetype = Variable
      )
    ) +
    geom_line(size = 0.75) + geom_point(size = 5) +
    scale_shape_manual(values = c(22, 24, 25, 21)) +
    scale_color_manual(values = c("#56B4E9", "#009E73", "#CC79A7", "#D55E00")) +
    scale_fill_manual(values = c("#56B4E950", "#009E7350", "#CC79A750", "#D55E0050")) +
    scale_linetype_manual(values = c(5, 5, 5, 1)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "", breaks = seq(1, 8)) +
    theme_minimal_hgrid() +
    panel_border(size = 1.5) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Ordinalization")

  plot2 <-
    beliaeff2002 %>% gather(Variable, Value, Bmk1:Index) %>%
    ggplot(
      aes(
        x = Exposure,
        y = Value,
        group = Variable,
        color = Variable,
        fill = Variable,
        shape = Variable,
        linetype = Variable
      )
    ) +
    geom_line(size = 0.75) + geom_point(size = 5) +
    scale_shape_manual(values = c(22, 24, 25, 21)) +
    scale_color_manual(values = c("#56B4E9", "#009E73", "#CC79A7", "#D55E00")) +
    scale_fill_manual(values = c("#56B4E950", "#009E7350", "#CC79A750", "#D55E0050")) +
    scale_linetype_manual(values = c(5, 5, 5, 1)) +
    scale_y_continuous(name = "", limits = c(0, 4)) +
    theme_minimal_hgrid() +
    panel_border(size = 1.5) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Standardization")

  plot3 <-
    pham2021 %>% gather(Variable, Value, Bmk1:`ps-index`) %>%
    mutate(Variable = factor(
      Variable,
      levels = c("Bmk1",       "Bmk2",       "Bmk3",       "Unweighted index", "ps-index")
    )) %>%
    ggplot(
      aes(
        x = Exposure,
        y = Value,
        group = Variable,
        color = Variable,
        fill = Variable,
        shape = Variable,
        linetype = Variable
      )
    ) +
    geom_line(size = 0.75) + geom_point(size = 5) +
    scale_shape_manual(values = c(22, 24, 25, 21, 21)) +
    scale_color_manual(values = c("#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#000000")) +
    scale_fill_manual(values = c(
      "#56B4E950",
      "#009E7350",
      "#CC79A750",
      "#D55E0050",
      "#00000050"
    )) +
    scale_linetype_manual(values = c(5, 5, 5, 1, 1)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "", limits = c(0, 1)) +
    theme_minimal_hgrid() +
    panel_border(size = 1.5) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Estimation")

  legend3 <- get_legend(
    plot3 +
      guides(color = guide_legend(nrow = 1)) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "center"
      )
  )

  prow <- plot_grid(plot1,
                    plot2,
                    plot3,
                    align = 'vh',
                    # hjust = -1,
                    nrow = 1)


  fig_compare <-
    plot_grid(prow,
              legend3,
              ncol = 1,
              rel_heights = c(1, .1))



  tab <-
    list(
      pop = pop,
      pop_long = pop_long,
      sam = sam,
      sam_long = sam_long,
      m = m,
      blaise2002 = blaise2002,
      beliaeff2002 = beliaeff2002,
      eco = eco,
      pham2021 = pham2021
    )
  fig <- list (fig_sam = fig_sam, fig_compare = fig_compare)
  list(tab = tab, fig = fig, est = estimation)
}
