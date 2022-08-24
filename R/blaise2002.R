#' Compute Rank Sum Biomarker Index
#'
#' \code{blaise2002} calculates RSI in the hypothetical case study. This is not
#' meant to be called directly.
#'
#' @param sam a data frame, the first output of \code{\link{simul}}.
#' @param sam_mean a data frame, the third output of \code{\link{simul}}.
#'
#' @references Blaise, C., Gagné, F., Pellerin, J., Hansen, P.-D., & Trottier,
#'   S. (2002). Molluscan shellfish biomarker study of the Quebec, Canada,
#'   Saguenay Fjord with the soft-shell clam, Mya arenaria. Environmental
#'   Toxicology, 17(3), 170–186. \doi{10.1002/tox.10048}.
#'
#' @return \code{blaise2002} returns a data frame of RSI.
#'
#' @export
blaise2002 <- function(sam, sam_mean){
  Bmk1 <- Bmk2 <- Bmk3 <- Bmk4 <- Bmk5 <- NULL #no visible binding for global variable

  #Bmk1
  sorting <- sam_mean$Site[order(sam_mean$mean.Bmk1)] #stimu
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk1[sam$Site == sorting[i]],
                               sam$Bmk1[sam$Site == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df1 <- data.frame(sorting, Bmk1 = rank)

  #Bmk2
  sorting <- sam_mean$Site[order(-sam_mean$mean.Bmk2)] #inhibit
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk2[sam$Site == sorting[i]],
                               sam$Bmk2[sam$Site == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df2 <- data.frame(sorting, Bmk2 = rank)

  #Bmk3
  sorting <- sam_mean$Site[order(sam_mean$mean.Bmk3)] #stimu
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk3[sam$Site == sorting[i]],
                               sam$Bmk3[sam$Site == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df3 <- data.frame(sorting, Bmk3 = rank)

  #Bmk4
  sorting <- sam_mean$Site[order(-sam_mean$mean.Bmk4)] #inhibit
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk4[sam$Site == sorting[i]],
                               sam$Bmk4[sam$Site == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df4 <- data.frame(sorting, Bmk4 = rank)

  #Bmk5
  sorting <- sam_mean$Site[order(sam_mean$mean.Bmk5)] #stimu
  rank <- rep(1, 5)
  for (i in 1:4) {
    test.result <- wilcox.test(sam$Bmk5[sam$Site == sorting[i]],
                               sam$Bmk5[sam$Site == sorting[i + 1]])
    if (test.result$p.value <= 0.05)
      rank[i + 1] <- rank[i] + 1
    else
      rank[i + 1] <- rank[i]
  }
  df5 <- data.frame(sorting, Bmk5 = rank)

  #Index
  idx <-
    list(df1, df2, df3, df4, df5) %>% reduce(full_join, by="sorting") %>%
    rowwise %>% mutate(Index = sum(Bmk1, Bmk2, Bmk3, Bmk4, Bmk5)) %>% rename(Site = sorting)
  idx
}
