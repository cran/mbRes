#' Compute and Visualize ps-index
#'
#' \code{mbr} calculates the ps-index and visualizes the whole process.
#'
#' @param df a data frame with the name of experimental groups or biomonitoring
#'   sites as the first column and the measurement of biomarkers as the
#'   remaining columns.
#' @param df2 a data frame with the name of biomarkers as the first column and
#'   the ecological relevance as the second column.
#' @param rotate a logical, whether to rotate the biomarker labels in figures.
#' @param display a logical, whether to display cell values in heatmaps.
#'
#' @details The header of the first column can be any character, for example,
#'   'group' or 'site'. The first name appearing in the first column will
#'   determine the control group or the reference site. The other names will be
#'   treatment groups or test sites. The header of the remaining columns will
#'   define the list of biomarkers. \cr \cr The row order of biomarkers in
#'   \code{df2} must match the column order of biomarkers in \code{df}. If
#'   \code{df2} is missing, all biomarkers automatically have ecological
#'   relevance of only 1.
#'
#' @return \code{mbr} returns a list of length 4: \item{\code{input}}{a list of
#'   length 3 \describe{ \item{\code{main}}{the input biomarker data frame}
#'   \item{\code{extra}}{a data frame with 2 columns:\describe{
#'   \item{\code{biomarker}}{individual biomarker} \item{\code{eco}}{the input
#'   ecological relevance} }} \item{\code{eco.mean}}{the mean ecological
#'   relevance} }}\item{\code{es}}{a data frame with 14 columns:\describe{
#'   \item{\code{treatment}}{treatment groups or test sites}
#'   \item{\code{control}}{control group or reference site}
#'   \item{\code{tm_size}}{the sample size of treatment group}
#'   \item{\code{ct_size}}{the sample size of control group}
#'   \item{\code{biomarker}}{individual biomarker} \item{\code{delta}}{the
#'   Cliff's delta of treatment group} \item{\code{delta.abs}}{the absolute
#'   Cliff's delta} \item{\code{pval}}{the P-Value} \item{\code{sval}}{the
#'   surprisal or S-Value} \item{\code{se}}{the standard error of Cliff's delta}
#'   \item{\code{ci.lower}}{the lower bound of the confidence interval}
#'   \item{\code{ci.upper}}{the uppper bound of the confidence interval}
#'   \item{\code{ciw}}{the width of the confidence interval}
#'   \item{\code{eco}}{the ecological relevance of the biomarker}
#'   \item{\code{sval.cdf}}{the ascending rank of S-value by CDF method, see
#'   \code{\link{cume_dist}}} \item{\code{ciw.cdf}}{the descending rank of
#'   confidence interval width by CDF method} \item{\code{eco.cdf}}{the
#'   ascending rank of ecological relevance by CDF method}
#'   \item{\code{weight}}{the total weighting factor}}} \item{\code{idx}}{a data
#'   frame with 2 columns:\describe{ \item{\code{treatment}}{treatment groups or
#'   test sites} \item{\code{ps}}{the ps-index} }} \item{\code{fig}}{a list of 6
#'   ggplot objects\describe{ \item{\code{delta}}{the Cliff's delta}
#'   \item{\code{sval}}{the surprisal or S-value} \item{\code{ciw}}{confidence
#'   interval width} \item{\code{eco}}{the ecological relevance}
#'   \item{\code{weight}}{the weighting factor} \item{\code{ps}}{the ps-index}
#'   }}
#'
#' @examples
#' \donttest{mbr(sokolova2021, ecorelevance, rotate = TRUE)} #might take more than 5s in some machines
#'
#' @export
mbr <- function(df,
                df2 = NULL,
                rotate = FALSE,
                display = TRUE) {
  treatment <- biomarker <-
    delta <- pval <- sval <- se <-
    ci.lower <- ci.upper  <- ciw <-
    sval.cdf <- ciw.cdf <- eco.cdf <-
    delta.abs <- weight <-
    NULL #no visible binding for global variable

  grs <- unique(df[, 1]) %>% unlist %>% as.character()
  ct <- grs[1]
  tms <- grs[-1]
  cell1 <- colnames(df)[1]
  bmks <- colnames(df)[-1]

  if (missing(df2)) {
    eco <- rep(1, length(bmks))
  } else {
    bmks2 <- df2[1] %>% unlist() %>% unname()
    if (!identical(bmks, bmks2))
      stop("lists of biomarkers in two data frames do not match \n")
    eco <- df2[2] %>% unlist() %>% unname()
  }

  eco.mean <- mean(eco)

  input <-
    list(
      main = df,
      extra = data.frame(biomarker = factor(bmks, bmks), eco),
      eco.mean = eco.mean
    )

  message(paste(
    c(
      "Control group or reference site:",
      ct,
      "\nOrdered list of treatment groups or test sites:",
      tms,
      "\nOrdered list of biomarkers:",
      bmks,
      "\nCorresponding ecological relevance of biomarkers:",
      eco,
      "\nMean ecological relevance:",
      round(eco.mean, 2)
    ),
    collapse = " "
  ))

  es <- map(bmks, function(x) {
    s3 <-
      df %>% select(!!sym(cell1),!!sym(x)) %>% split(df[cell1])
    map(tms, function(y) {
      vec1 <- s3[[y]][2] %>% unlist %>% na.omit
      vec0 <- s3[[ct]][2] %>% unlist %>% na.omit

      resampling(vec1, vec0) %>%
        transmute(
          treatment = y,
          control = ct,
          tm_size = length(vec1),
          ct_size = length(vec0),
          biomarker = x,
          delta,
          delta.abs = abs(delta),
          pval,
          sval,
          se,
          ci.lower,
          ci.upper,
          ciw,
          eco = eco[match(x, bmks)]
        )
    }) %>%  rbindlist()
  }) %>%  rbindlist() %>% mutate(treatment = factor(treatment, tms),
                                 biomarker = factor(biomarker, bmks)) %>%
    mutate(
      sval.cdf =  cume_dist(sval),
      ciw.cdf = cume_dist(-ciw),
      eco.cdf =  cume_dist(eco)
    )  %>% mutate(weight = 0.25 * sval.cdf + 0.25 * ciw.cdf + 0.5 * eco.cdf)

  idx <-
    es %>% group_by(treatment) %>% summarise(ps = weighted.mean(delta.abs, weight))

  fig.delta <-
    ggheat(
      es,
      "biomarker",
      "treatment",
      "delta",
      "Cliff's delta",
      c(-1, 1),
      "#0072B2",
      "#D55E00",
      diverging = TRUE
    )
  fig.sval <-
    ggheat(es,
           "biomarker",
           "treatment",
           "sval",
           "Surprisal",
           c(0, NA),
           "white",
           "#56B4E9")
  fig.ciw <-
    ggheat(
      es,
      "biomarker",
      "treatment",
      "ciw",
      "Confidence interval width",
      c(0, NA),
      "#F0E442",
      "white",
    )
  fig.eco <- ggbar(input$extra, "biomarker", "eco", eco.mean)
  fig.weight <-
    ggheat(es,
           "biomarker",
           "treatment",
           "weight",
           "Weighting factor",
           c(0, 1),
           "white",
           "#CC79A7")
  fig.ps <- ggdot(idx, "ps", "treatment")

  fig <- list(
    delta = fig.delta,
    sval = fig.sval,
    ciw = fig.ciw,
    eco = fig.eco,
    weight = fig.weight,
    ps = fig.ps
  )

  list(
    input = input,
    es = es,
    idx = idx,
    fig = fig
  )
}
