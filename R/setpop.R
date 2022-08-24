#' Define Hypothetical Populations
#'
#' \code{setpop} sets the true means of biomarker responses in populations. This
#' is used for the hypothetical case study.
#'
#' @return \code{setpop} returns a list of length 3:
#' \item{\code{pop_mean}}{true means of biomarker responses in populations.}
#' \item{\code{pop_mean_long}}{true means of biomarker responses in long format.}
#' \item{\code{pop_profile}}{profile of biomarkers.}
#'
#' @export
setpop <- function() {

  Biomarker <- Mean_Response <- Bmk1 <- Bmk5 <- NULL #global binding error

  pop_mean <- data.frame(
    Site = c("S0", "S1", "S2", "S3", "S4"),
    Bmk1 = c(0, 0.5, 1, 1.5, 2),
    Bmk2 = c(2, 1.75, 1, 0.25, 0),
    Bmk3 = c(0, 1.5, 2, 1.5, 0),
    Bmk4 = c(1.5, 2, 1, 0.5, 0),
    Bmk5 = c(0, 2, 1, 1.5, 0)
  )
  pop_mean_long <-
    pop_mean %>% gather(Biomarker, Mean_Response, Bmk1:Bmk5)

  pop_profile <-
    data.frame(
      Biomarker = c("Bmk1", "Bmk2", "Bmk3", "Bmk4", "Bmk5"),
      Profile = c(
        "linear",
        "sigmoid",
        "inverted U-shaped",
        "inverted J-shaped",
        "multiphasic"
      )
    )

  list(
    pop_mean = pop_mean,
    pop_mean_long = pop_mean_long,
    pop_profile = pop_profile
  )
}
