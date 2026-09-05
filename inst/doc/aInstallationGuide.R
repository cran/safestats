## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## ----remotes, eval=FALSE------------------------------------------------------
# install.packages("remotes")
# library("remotes")

## ----eval=FALSE---------------------------------------------------------------
# remotes::install_github("AlexanderLyNL/safestats", ref = "futility88")
# library(safestats)

## ----eval=FALSE---------------------------------------------------------------
# install.packages(path_to_file, repos = NULL, type="source")

## ----eval=FALSE---------------------------------------------------------------
# # # PSEUDO CODE
# # #   This won't work, but it just a way to explain the ideas
# # designObj <- designSaviAnalysis(alternative="twoSided")
# # result <- saviAnalysis(x=dat$x, designObj)

## -----------------------------------------------------------------------------
xDomain <- seq(80, 150)
treatmentPopulation <- dnorm(xDomain, mean=122, sd=12)
controlPopulation <- dnorm(xDomain, mean=112, sd=12)

oldPar <- safestats::setSafeStatsPlotOptionsAndReturnOldOnes();
plot(xDomain, treatmentPopulation, type="l", lwd=2, col="red",
     ylab="Density", xlab="IQ Scores")
lines(xDomain, controlPopulation, type="l", lwd=2, col="blue")
lines(c(122, 122), c(0, 1), lty=3)
lines(c(112, 112), c(0, 1), lty=3)

## -----------------------------------------------------------------------------
xDomain <- seq(-20, 40)
# variance of the sum X+(-Y) is the sum of the variances
differencePopulationAlt <- dnorm(xDomain, mean=10, sd=sqrt(2)*12)
differencePopulationNull <- dnorm(xDomain, mean=0, sd=sqrt(2)*12)

oldPar <- safestats::setSafeStatsPlotOptionsAndReturnOldOnes();
plot(xDomain, differencePopulationAlt, type="l", lwd=2,
     col="black", ylab="Density", xlab="IQ Scores difference")
lines(xDomain, differencePopulationNull, lwd=2, lty=2)
lines(c(0, 0), c(0, 1), lwd=1, lty=3)
lines(c(10, 10), c(0, 1), lwd=1, lty=3)

## ----eval=FALSE---------------------------------------------------------------
# library(safestats)
# sigma <- 12
# designObj <- designSaviZ(meanDiffMin=10,
#                          power=0.8, sigma=sigma,
#                          testType="twoSample", seed=1)
# designObj

## ----eval=FALSE---------------------------------------------------------------
# set.seed(2)
# treatmentGroup <- rnorm(36, mean=122, sd=sigma)
# controlGroup <- rnorm(36, mean=112, sd=sigma)
# 
# resultObj <- saviZTest(x=treatmentGroup, y=controlGroup,
#                        designObj=designObj)
# resultObj
# 
# plot(resultObj)
# plot(resultObj, wantConfSeqPlot=TRUE, log=FALSE)

