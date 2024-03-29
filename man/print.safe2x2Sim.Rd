% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe2x2Test.R
\name{print.safe2x2Sim}
\alias{print.safe2x2Sim}
\title{Prints Results of Simulations for Comparing Hyperparameters for Safe Tests of Two Proportions}
\usage{
\method{print}{safe2x2Sim}(x, ...)
}
\arguments{
\item{x}{a result object obtained through \code{\link{simulateTwoProportions}()}.}

\item{...}{further arguments to be passed to or from methods.}
}
\value{
The data frame with simulation results, called for side effects to pretty print the simulation results.
}
\description{
Prints Results of Simulations for Comparing Hyperparameters for Safe Tests of Two Proportions
}
\examples{
priorList1 <- list(betaA1 = 10, betaA2 = 1, betaB1 = 1, betaB2 = 10)
priorList2 <- list(betaA1 = 0.18, betaA2 = 0.18, betaB1 = 0.18, betaB2 = 0.18)
priorList3 <- list(betaA1 = 1, betaA2 = 1, betaB1 = 1, betaB2 = 1)

simResult <- simulateTwoProportions(
  hyperparameterList = list(priorList1, priorList2, priorList3),
  alternativeRestriction = "none",
  alpha = 0.1, beta = 0.2, na = 1, nb = 1,
  deltamax = -0.4, deltamin = -0.9, deltaGridSize = 3,
  M = 10
  )
}
