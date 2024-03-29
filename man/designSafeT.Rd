% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tTest.R
\name{designSafeT}
\alias{designSafeT}
\title{Designs a Safe Experiment to Test Means with a T Test}
\usage{
designSafeT(
  deltaMin = NULL,
  beta = NULL,
  nPlan = NULL,
  alpha = 0.05,
  h0 = 0,
  alternative = c("twoSided", "greater", "less"),
  lowN = 3L,
  highN = 1000000L,
  lowParam = 0.01,
  highParam = 1.5,
  tol = 0.01,
  testType = c("oneSample", "paired", "twoSample"),
  ratio = 1,
  nSim = 1000L,
  nBoot = 1000L,
  parameter = NULL,
  pb = TRUE,
  seed = NULL,
  ...
)
}
\arguments{
\item{deltaMin}{numeric that defines the minimal relevant standardised effect size, the smallest effect size that
we would the experiment to be able to detect.}

\item{beta}{numeric in (0, 1) that specifies the tolerable type II error control necessary to calculate both
the sample sizes and deltaS, which defines the test. Note that 1-beta defines the power.}

\item{nPlan}{vector of max length 2 representing the planned sample sizes.}

\item{alpha}{numeric in (0, 1) that specifies the tolerable type I error control --independent of n-- that the
designed test has to adhere to. Note that it also defines the rejection rule e10 > 1/alpha.}

\item{h0}{a number indicating the hypothesised true value of the mean under the null. For the moment h0=0.}

\item{alternative}{a character string specifying the alternative hypothesis must be one of "twoSided" (default),
"greater" or "less".}

\item{lowN}{integer minimal sample size of the (first) sample when computing the power due to
optional stopping. Default lowN is set 1.}

\item{highN}{integer minimal sample size of the (first) sample when computing the power due to
optional stopping. Default highN is set 1e6.}

\item{lowParam}{numeric defining the smallest delta of the search space for the test-defining deltaS
for scenario 3. Currently not yet in use.}

\item{highParam}{numeric defining the largest delta of the search space for the test-defining deltaS
for scenario 3. Currently not yet in use.}

\item{tol}{a number that defines the stepsizes between the lowParam and highParam.}

\item{testType}{either one of "oneSample", "paired", "twoSample".}

\item{ratio}{numeric > 0 representing the randomisation ratio of condition 2 over condition 1. If testType
is not equal to "twoSample", or if nPlan is of length(1) then ratio=1.}

\item{nSim}{integer > 0, the number of simulations needed to compute power or the number of samples paths
for the safe z test under continuous monitoring.}

\item{nBoot}{integer > 0 representing the number of bootstrap samples to assess the accuracy of
approximation of the power, the number of samples for the safe z test under continuous monitoring,
or for the computation of the logarithm of the implied target.}

\item{parameter}{optional test defining parameter. Default set to \code{NULL}.}

\item{pb}{logical, if \code{TRUE}, then show progress bar.}

\item{seed}{integer, seed number.}

\item{...}{further arguments to be passed to or from methods, but mainly to perform do.calls.}
}
\value{
Returns an object of class 'safeDesign'. An object of class 'safeDesign' is a list containing at least the
following components:

\describe{
  \item{nPlan}{the planned sample size(s).}
  \item{parameter}{the safe test defining parameter. Here deltaS.}
  \item{esMin}{the minimal clinically relevant standardised effect size provided by the user.}
  \item{alpha}{the tolerable type I error provided by the user.}
  \item{beta}{the tolerable type II error provided by the user.}
  \item{alternative}{any of "twoSided", "greater", "less" provided by the user.}
  \item{testType}{any of "oneSample", "paired", "twoSample" provided by the user.}
  \item{paired}{logical, \code{TRUE} if "paired", \code{FALSE} otherwise.}
  \item{h0}{the specified hypothesised value of the mean or mean difference depending on
  whether it was a one-sample or a two-sample test.}
  \item{ratio}{default is 1. Different from 1, whenever testType equals "twoSample", then it defines
  ratio between the planned randomisation of condition 2 over condition 1.}
  \item{lowN}{the smallest n of the search space for n provided by the user.}
  \item{highN}{the largest n of the search space for n provided by the user.}
  \item{lowParam}{the smallest delta of the search space for delta provided by the user.}
  \item{highParam}{the largest delta of the search space for delta provided by the user.}
  \item{tol}{the step size between lowParam and highParam provided by the user.}
  \item{pilot}{\code{FALSE} (default) specified by the user to indicate that the design is not a pilot study.}
  \item{call}{the expression with which this function is called.}
}
}
\description{
A designed experiment requires (1) a sample size nPlan to plan for, and (2) the parameter of the safe test, i.e.,
deltaS. If nPlan is provided, then only the safe test defining parameter deltaS needs to determined. That resulting
deltaS leads to an (approximately) most powerful safe test. Typically, nPlan is unknown and the user has to specify
(i) a tolerable type II error beta, and (ii) a clinically relevant minimal population standardised effect size
deltaMin. The procedure finds the smallest nPlan for which deltaMin is found with power of at least 1 - beta.
}
\examples{
designObj <- designSafeT(deltaMin=0.8, alpha=0.03, alternative="greater")
designObj

# "Scenario 1.a": Minimal clinically relevant standarised mean difference and tolerable type
# II error also known. Goal: find nPlan.
designObj <- designSafeT(deltaMin=0.8, alpha=0.03, beta=0.4, nSim=10, alternative="greater")
designObj

# "Scenario 2": Minimal clinically relevant standarised mean difference and nPlan known.
# Goal: find the power, hence, the type II error of the procedure under optional stopping.

designObj <- designSafeT(deltaMin=0.8, alpha=0.03, nPlan=16, nSim=10, alternative="greater")
designObj
}
