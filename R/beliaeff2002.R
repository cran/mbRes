#' Compute Integrated Biomarker Index
#'
#' \code{beliaeff2002} calculates IBR in the hypothetical case study. This is
#' not meant to be called directly.
#'
#' @param sam_mean a data frame, the third output of \code{\link{simul}}.
#'
#' @references Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker
#'   response: A useful tool for ecological risk assessment. Environmental
#'   Toxicology and Chemistry, 21(6), 1316–1322. \doi{10.1002/etc.5620210629}.
#'
#' @return \code{beliaeff2002} returns a data frame of IBR.
#'
#' @export
beliaeff2002 <- function(sam_mean){
  mean.Bmk1 <-
    Site <- zscore <- mean.Bmk2 <- mean.Bmk3 <- mean.Bmk4 <-
    mean.Bmk5 <-
    Bmk1 <-
    Bmk2 <-
    Bmk3 <-
    Bmk4 <- Bmk5 <- NULL #no visible binding for global variable

  #Bmk1
  pool <- sam_mean %>% summarise(mean = mean(mean.Bmk1),
                          sd = sd(mean.Bmk1))
  temp <- sam_mean %>% select(Site, mean.Bmk1) %>% rowwise %>%
    mutate(zscore = (mean.Bmk1 - pool$mean) / pool$sd) #stimu
  dat1 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))

  #Bmk2
  pool <- sam_mean %>% summarise(mean = mean(mean.Bmk2),
                          sd = sd(mean.Bmk2))
  temp <- sam_mean %>% select(Site, mean.Bmk2) %>% rowwise %>%
    mutate(zscore = -(mean.Bmk2 - pool$mean) / pool$sd) #inhibition
  dat2 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))

  #Bmk3
  pool <- sam_mean %>% summarise(mean = mean(mean.Bmk3),
                          sd = sd(mean.Bmk3))
  temp <- sam_mean %>% select(Site, mean.Bmk3) %>% rowwise %>%
    mutate(zscore = (mean.Bmk3 - pool$mean) / pool$sd) #stimu
  dat3 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))

  #Bmk4
  pool <- sam_mean %>% summarise(mean = mean(mean.Bmk4),
                                 sd = sd(mean.Bmk4))
  temp <- sam_mean %>% select(Site, mean.Bmk4) %>% rowwise %>%
    mutate(zscore = -(mean.Bmk4 - pool$mean) / pool$sd) #inhibition
  dat4 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))

  #Bmk5
  pool <- sam_mean %>% summarise(mean = mean(mean.Bmk5),
                                 sd = sd(mean.Bmk5))
  temp <- sam_mean %>% select(Site, mean.Bmk5) %>% rowwise %>%
    mutate(zscore = (mean.Bmk5 - pool$mean) / pool$sd) #stimu
  dat5 <-
    temp %>% mutate(trans.score = zscore + abs(min(temp$zscore)))

  #Index
  idx <-
    data.frame(
      Site = dat1$Site,
      Bmk1 = dat1$trans.score,
      Bmk2 = dat2$trans.score,
      Bmk3 = dat3$trans.score,
      Bmk4 = dat4$trans.score,
      Bmk5 = dat5$trans.score
    ) %>%
    mutate(
      Index = 0.5 * Bmk1 * Bmk2 * sin(72 * pi / 180) +
        0.5 * Bmk2 * Bmk3 * sin(72 * pi / 180) +
        0.5 * Bmk3 * Bmk4 * sin(72 * pi / 180) +
      0.5 * Bmk4 * Bmk5 * sin(72 * pi / 180) +
      0.5 * Bmk5 * Bmk1 * sin(72 * pi / 180)
    )
  idx
}
