% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blaise2002.R
\name{blaise2002}
\alias{blaise2002}
\title{Compute Rank Sum Biomarker Index}
\usage{
blaise2002(sam, sam_mean)
}
\arguments{
\item{sam}{a data frame, the first output of \code{\link{simul}}.}

\item{sam_mean}{a data frame, the third output of \code{\link{simul}}.}
}
\value{
\code{blaise2002} returns a data frame of RSI.
}
\description{
\code{blaise2002} calculates RSI in the hypothetical case study. This is not
meant to be called directly.
}
\references{
Blaise, C., Gagné, F., Pellerin, J., Hansen, P.-D., & Trottier,
  S. (2002). Molluscan shellfish biomarker study of the Quebec, Canada,
  Saguenay Fjord with the soft-shell clam, Mya arenaria. Environmental
  Toxicology, 17(3), 170–186. \doi{10.1002/tox.10048}.
}
