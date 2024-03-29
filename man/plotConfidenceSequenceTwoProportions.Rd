% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe2x2Test.R
\name{plotConfidenceSequenceTwoProportions}
\alias{plotConfidenceSequenceTwoProportions}
\title{Plot bounds of a safe confidence sequence of the difference or log odds ratio for two proportions
against the number of data blocks in two data streams ya and yb.}
\usage{
plotConfidenceSequenceTwoProportions(
  ya,
  yb,
  safeDesign,
  differenceMeasure = c("difference", "odds"),
  precision = 100,
  deltaStart = 0.001,
  deltaStop = 3,
  trueDifference = NA
)
}
\arguments{
\item{ya}{positive observations/ events per data block in group a: a numeric with integer values
between (and including) 0 and \code{na}, the number of observations in group a per block.}

\item{yb}{positive observations/ events per data block in group b: a numeric with integer values
between (and including) 0 and \code{nb}, the number of observations in group b per block.}

\item{safeDesign}{a safe test design for two proportions retrieved through \code{\link{designSafeTwoProportions}()}.}

\item{differenceMeasure}{the difference measure to construct the confidence interval for:
one of "difference" and "odds".}

\item{precision}{precision of the grid to search over for the confidence sequence bounds.}

\item{deltaStart}{for the odds difference measure: the (absolute value of the) smallest
log odds ratio to assess for in- or exclusion in the confidence sequence. Default 0.001.}

\item{deltaStop}{for the odds difference measure: the (absolute value of the) highest
log odds ratio to assess for in- or exclusion in the confidence sequence. Default 3.}

\item{trueDifference}{true difference or log odds ratio in groups A and B: added to the plot.}
}
\value{
no return value; called for its side effects, a plot of the confidence sequence.
}
\description{
Plot bounds of a safe confidence sequence of the difference or log odds ratio for two proportions
against the number of data blocks in two data streams ya and yb.
}
\examples{
set.seed(39413)
ya <- rbinom(n = 30, size = 1, prob = 0.1)
yb <- rbinom(n = 30, size = 1, prob = 0.8)
balancedSafeDesign <- designSafeTwoProportions(na = 1,
                                               nb = 1,
                                               nBlocksPlan = 30)
plotConfidenceSequenceTwoProportions(ya = ya,
                                     yb = yb,
                                     safeDesign = balancedSafeDesign,
                                     differenceMeasure = "difference",
                                     precision = 15,
                                     trueDifference = 0.7)

#log odds ratio difference measure
plotConfidenceSequenceTwoProportions(ya = ya,
                                     yb = yb,
                                     safeDesign = balancedSafeDesign,
                                     differenceMeasure = "odds",
                                     precision = 15,
                                     deltaStop = 5,
                                     trueDifference = log(36))

#switch ya and yb: observe negative log odds ratio in the data, plot mirrored in x-axis
plotConfidenceSequenceTwoProportions(ya = yb,
                                     yb = ya,
                                     safeDesign = balancedSafeDesign,
                                     differenceMeasure = "odds",
                                     precision = 15,
                                     deltaStop = 5,
                                     trueDifference = -log(36))

}
