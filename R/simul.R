#' Generate Hypothetical Samples
#'
#' \code{simul} yields a sample dataset of biomarker responses. This
#' is used for the hypothetical case study.
#'
#' @param pop_mean a data frame, the first output of \code{setpop}.
#' @param size an integer, the sample size.
#'
#' @return \code{simul} returns a list of length 3:
#' \item{\code{sam}}{sample dataset.}
#' \item{\code{sam_long}}{sample dataset in long format.}
#' \item{\code{sam_mean}}{sample means of biomarker responses.}
#'
#' @export
simul <- function(pop_mean, size = 75) {
  Biomarker <- Response <- Site <- Bmk1 <-
    Bmk2 <- Bmk3 <- Bmk4 <- Bmk5 <- NULL #no visible binding for global variable


  sam <-
    data.frame(
      Site = rep(pop_mean$Site, each = size),
      Bmk1 = sapply(pop_mean$Bmk1, rnorm, n = size, sd = 1) |> as.vector(),
      Bmk2 = sapply(pop_mean$Bmk2, rnorm, n = size, sd = 1) |> as.vector(),
      Bmk3 = sapply(pop_mean$Bmk3, rnorm, n = size, sd = 1) |> as.vector(),
      Bmk4 = sapply(pop_mean$Bmk4, rnorm, n = size, sd = 1) |> as.vector(),
      Bmk5 = sapply(pop_mean$Bmk5, rnorm, n = size, sd = 1) |> as.vector()

    )

  sam_long <- sam %>% gather(Biomarker, Response, Bmk1:Bmk5)

  #mean by group
  sam_mean <- sam %>% group_by(Site) %>% summarise(
    mean.Bmk1 = mean(Bmk1),
    mean.Bmk2 = mean(Bmk2),
    mean.Bmk3 = mean(Bmk3),
    mean.Bmk4 = mean(Bmk4),
    mean.Bmk5 = mean(Bmk5)
  )

  list(sam = sam,
       sam_long = sam_long,
       sam_mean = sam_mean)
}
