#' Compute Cliff's delta simplified
#'
#' \code{mbr.cliff} summarizes Cliff's delta for multiple groups and
#' multiple biomarkers.
#'
#' @param df a data frame with the name of experimental groups or biomonitoring
#'   sites as the first column and the measurement of biomarkers as the
#'   remaining columns.
#'
#' @examples
#'
#' \donttest{
#' set.seed(1)
#' setting <- setpop()
#' temp <- simul(setting$pop_mean)
#' mbr.cliff(temp$sam)
#' } #might take more than 5s in some machines
#'
#' @export
mbr.cliff <- function(df) {

  biomarker <- test_site <-
    delta <- delta.abs <-
    NULL #no visible binding for global variable

  cliff <- function(v1, v0) {
    if (any(is.na(c(v1, v0))))
      stop("missing values detected in the vectors \n")
    rnk <- rank(c(v1, v0), ties.method = "average")
    r1 <- sum(rnk[seq_along(v1)])
    n1 <- length(v1)
    n0 <- length(v0)

    delta <- (2 * r1 - n1 * (n1 + 1) - n1 * n0) / (n1 * n0)
    data.frame(delta)
  }


  grs <- unique(df[, 1]) %>% unlist %>% as.character()
  ref <- grs[1]
  tests <- grs
  cell1 <- colnames(df)[1]
  bmks <- colnames(df)[-1]

  mess <- c(paste(c("Reference site (or control group):",
                    ref),
                  collapse = " "),
            paste(
              c("Ordered list of test sites (or treatment groups):",
                tests),
              collapse = " "
            ),
            paste(c("Ordered list of biomarkers:",
                    bmks),
                  collapse = " "))

  es <- map(bmks, function(x) {
    s3 <-
      df %>% select(!!sym(cell1), !!sym(x)) %>% split(df[cell1])
    map(tests, function(y) {
      vec1 <- s3[[y]][2] %>% unlist %>% na.omit
      vec0 <- s3[[ref]][2] %>% unlist %>% na.omit

      cliff(vec1, vec0) %>%
        transmute(
          test_site = y,
          ref_site = ref,
          t_size = length(vec1),
          r_size = length(vec0),
          biomarker = x,
          delta,
          delta.abs = abs(delta)
        )
    }) %>%  rbindlist()
  }) %>%  rbindlist() %>% mutate(test_site = factor(test_site, tests),
                                 biomarker = factor(biomarker, bmks))

  fig.delta <-     ggplot(es,
             aes(
               x = biomarker,
               y = test_site,
               fill = delta
             )) +
        geom_tile(color = "white",
                  size = 0.25) +
                      geom_text(aes(label = round(delta, 2)), colour = "black")+
        scale_x_discrete(name = NULL,
                         expand = c(0, 0),
                         position = "top") +
        scale_y_discrete(name = NULL, limits = rev) +
            scale_fill_gradient2(
              low = "#0072B2",
              high = "#CC79A7",
              limits = c(-1, 1),
              name = NULL
            )+
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
        labs(title = "Cliff's delta") +
        theme_cowplot() +
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



  list(mess = mess,
       es = es,
       fig.delta = fig.delta)
}



