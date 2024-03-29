% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tTest.R
\name{computeConfidenceIntervalT}
\alias{computeConfidenceIntervalT}
\title{Helper function: Computes the safe confidence sequence for the mean in a t-test}
\usage{
computeConfidenceIntervalT(
  meanObs,
  sdObs,
  nEff,
  nu,
  deltaS,
  ciValue = 0.95,
  g = NULL
)
}
\arguments{
\item{meanObs}{numeric, the observed mean. For two sample tests this is difference of the means.}

\item{sdObs}{numeric, the observed standard deviation. For a two-sample test this is the root
of the pooled variance.}

\item{nEff}{numeric > 0, the effective sample size. For one sample test this is just n.}

\item{nu}{numeric > 0, the degrees of freedom.}

\item{deltaS}{numeric > 0, the safe test defining parameter.}

\item{ciValue}{numeric is the ciValue-level of the confidence sequence. Default ciValue=0.95.}

\item{g}{numeric > 0, used as the variance of the normal prior on the population delta
Default is \code{NULL} in which case g=delta^2.}
}
\value{
numeric vector that contains the upper and lower bound of the safe confidence sequence
}
\description{
Helper function: Computes the safe confidence sequence for the mean in a t-test
}
\examples{
computeConfidenceIntervalT(meanObs=0.3, sdObs=2, nEff=12, nu=11, deltaS=0.4)
}
