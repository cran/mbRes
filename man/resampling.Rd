% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resampling.R
\name{resampling}
\alias{resampling}
\title{Measure Statistical Uncertainty}
\usage{
resampling(v1, v0, nrand = 1999, seed = 1)
}
\arguments{
\item{v1}{a vector, biomarker values from the treatment group.}

\item{v0}{a vector, biomarker values from the control group.}

\item{nrand}{an integer, the number of randomization samples. The default
value is 1999.}

\item{seed}{an integer, the seed for random number generation. Setting a seed
ensures the reproducibility of the result. See \code{\link{set.seed}} for
more details.}
}
\value{
\code{resampling} returns a one-row data frame with 3 numerics:
  \item{\code{delta}}{the Cliff's delta of the treatment group.}
  \item{\code{pval}}{the observed P-value p under the null hypothesis.}
  \item{\code{sval}}{the S-value s calculated from P-value p.}
}
\description{
\code{resampling} performs randomization test to calculate P-value and
S-value.
}
\examples{
set.seed(1)
setting <- setpop()
temp <- simul(setting$pop_mean)
resampling(subset(temp$sam, Site == "S1", Bmk1, drop = TRUE),
subset(temp$sam, Site == "S0", Bmk1, drop = TRUE))

}
\references{
Greenland, S. (2019). Valid P-Values Behave Exactly as They
  Should: Some Misleading Criticisms of P-Values and Their Resolution With
  S-Values. The American Statistician, 73(sup1), 106–114.
  \doi{10.1080/00031305.2018.1529625}. \cr \cr Phipson, B., & Smyth, G. K.
  (2010). Permutation P-values Should Never Be Zero: Calculating Exact
  P-values When Permutations Are Randomly Drawn. Statistical Applications in
  Genetics and Molecular Biology, 9(1). \doi{10.2202/1544-6115.1585}.
}
\seealso{
\code{\link[RProbSup]{A1}}.
}
