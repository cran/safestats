% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zTest.R
\name{safeZTestStat}
\alias{safeZTestStat}
\title{Computes E-Values Based on the Z-Statistic}
\usage{
safeZTestStat(
  z,
  phiS,
  n1,
  n2 = NULL,
  alternative = c("twoSided", "less", "greater"),
  paired = FALSE,
  sigma = 1,
  ...
)
}
\arguments{
\item{z}{numeric that represents the observed z-statistic.}

\item{phiS}{numeric this defines the safe test S, i.e., a likelihood ratio of z distributions with in the
denominator the likelihood with mean difference 0 and in the numerator an average likelihood defined by
the likelihood at the parameter value. For the two sided case 1/2 at the parameter value and 1/2 at minus the
parameter value.}

\item{n1}{integer that represents the size in a one-sample z-test, (n2=\code{NULL}). When n2 is not
\code{NULL}, this specifies the size of the first sample for a two-sample test.}

\item{n2}{an optional integer that specifies the size of the second sample. If it's left unspecified, thus,
\code{NULL} it implies that the z-statistic is based on one-sample.}

\item{alternative}{a character string specifying the alternative hypothesis must be one of "twoSided" (default),
"greater" or "less".}

\item{paired}{a logical, if \code{TRUE} ignores n2, and indicates that a paired z-test is performed.}

\item{sigma}{numeric, the assumed known standard deviation, default 1.}

\item{...}{further arguments to be passed to or from methods.}
}
\value{
Returns an e-value.
}
\description{
Computes e-values using the z-statistic and the sample sizes only based on the test defining parameter phiS.
}
\examples{
safeZTestStat(z=1, n1=100, phiS=0.4)
safeZTestStat(z=3, n1=100, phiS=0.3)
}
