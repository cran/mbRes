#' Compute Cliff's delta and S-value
#'
#' \code{mbr} summarizes Cliff's delta and S-value for multiple groups and
#' multiple biomarkers.
#'
#' @param df a data frame with the name of experimental groups or biomonitoring
#'   sites as the first column and the measurement of biomarkers as the
#'   remaining columns.
#'
#' @details The header of the first column can be any character, for example,
#'   'group' or 'site'. The first name appearing in the first column will
#'   determine the control group or the reference site. The other names will be
#'   treatment groups or test sites. The header of the remaining columns will
#'   define the list of biomarkers.
#'
#' @return \code{mbr} returns a list of length 3: \item{\code{mess}}{a list of
#'   length 3 confirms the information about \code{df}.}\item{\code{es}}{a data
#'   frame with 9 columns:\describe{ \item{\code{test_site}}{treatment groups or
#'   test sites.} \item{\code{ref_site }}{control group or reference site.}
#'   \item{\code{t_size }}{the sample size of treatment group or test sites.}
#'   \item{\code{r_size }}{the sample size of control group or reference site.}
#'   \item{\code{biomarker}}{individual biomarker.} \item{\code{delta}}{the
#'   Cliff's delta of treatment group or reference site.}
#'   \item{\code{delta.abs}}{the absolute Cliff's delta.} \item{\code{pval}}{the
#'   P-Value.} \item{\code{sval}}{the surprisal or S-Value.}}}
#'   \item{\code{idx}}{a data frame summarizes \code{delta.abs} and their
#'   average.}
#'
#' @examples
#'
#' \donttest{
#' set.seed(1)
#' setting <- setpop()
#' temp <- simul(setting$pop_mean)
#' mbr(temp$sam)
#' } #might take more than 5s in some machines
#'
#' @export
mbr <- function(df) {
  biomarker <- test_site <-
    delta <- delta.abs <- pval <- sval <-
    NULL #no visible binding for global variable

  grs <- unique(df[, 1]) %>% unlist %>% as.character()
  ref <- grs[1]
  tests <- grs[-1]
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

      resampling(vec1, vec0) %>%
        transmute(
          test_site = y,
          ref_site = ref,
          t_size = length(vec1),
          r_size = length(vec0),
          biomarker = x,
          delta,
          delta.abs = abs(delta),
          pval,
          sval
        )
    }) %>%  rbindlist()
  }) %>%  rbindlist() %>% mutate(test_site = factor(test_site, tests),
                                 biomarker = factor(biomarker, bmks))

  idx <- merge(
    es %>% select(test_site, biomarker, delta) %>% spread(biomarker, delta),
    es %>% group_by(test_site) %>% summarise(avg = mean(delta.abs))
  ) %>% rename_with( ~ cell1, test_site)

  list(mess = mess,
       es = es,
       idx = idx)
}
