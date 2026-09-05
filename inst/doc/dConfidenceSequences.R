## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## ----eval=FALSE---------------------------------------------------------------
# library("safestats")
# freqColours <- c("#E31A1CE6", "#FB9A9980")
# bayesColours <- c("#B15928E6", "#FFFF9980")
# eColours <- c("#1F78B4E6", "#A6CEE380")
# alpha <- 0.05

## ----eval=FALSE---------------------------------------------------------------
# n <- 100
# muTrue <- 8
# sigma <- 1
# 
# set.seed(4)
# someDataSingle <- rnorm(n, mean=muTrue,
#                         sd=sigma)

## ----eval=FALSE---------------------------------------------------------------
# freqCi <- matrix(nrow=n, ncol=2)
# 
# meanVector <- 1/(1:n)*cumsum(someDataSingle)
# 
# for (i in 1:n) {
#   tempResult <- computeConfidenceIntervalZ(nEff=i,
#                                           meanObs=meanVector[i],
#                                           eType="freq", parameter=NULL)
#   freqCi[i, ] <- tempResult
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n, freqCi[, 1], xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# 
# polygon(c(1:n, n:1), c(freqCi[, 2], rev(freqCi[, 1])),
#         col=freqColours[2], border=freqColours[1], lwd=2,
#         density = NULL, angle = -20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval = FALSE-------------------------------------------------------------
# # The indices where the lower bound is above muTrue
# which(freqCi[, 1] > muTrue)
# 
# # No indices where the upper bound is below muTrue
# which(freqCi[, 2] < muTrue)

## ----eval=FALSE---------------------------------------------------------------
# mIter <- 1000
# 
# set.seed(1)
# allData <- matrix(rnorm(mIter*n, mean=muTrue), nrow=mIter)
# allFreqCis <- array(dim=c(mIter, n, 2))
# 
# 
# # This indicates whether a simulation yielded an interval that
# # does not cover the true mean
# freqCiError <- integer(mIter)
# 
# # This indicates the first time an interval does not cover the true mean
# firstPassageTime <- rep(Inf, times=mIter)
# 
# for (sim in 1:mIter) {
#   someData <- allData[sim, ]
#   meanVector <- 1/(1:n)*cumsum(someData)
# 
#   for (i in 1:n) {
#     tempResult <- computeConfidenceIntervalZ(
#       nEff=i, meanObs=meanVector[i], eType="freq",
#       parameter=NULL)
#     allFreqCis[sim, i, ] <- tempResult
# 
#     if ((tempResult[1] > muTrue || tempResult[2] < muTrue)
#         && freqCiError[sim] != 1) {
#       freqCiError[sim] <- 1
#       firstPassageTime[sim] <- i
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# complementCoverageRate <- numeric(n)
# 
# for (i in 1:n) {
#   complementCoverageRate[i] <- mean(firstPassageTime <= i)
# }
# 
# freqCoverageRate <- 1-complementCoverageRate
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n, 100*freqCoverageRate, type="l", lwd=2, xlab="n",
#      ylab="Coverage rate (%)", col=freqColours[1], ylim=c(60, 100))
# lines(c(1, n), 100*c(1-alpha, 1-alpha), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# failedIndeces <- which(freqCiError==1)
# 
# someIndex <- failedIndeces[3]
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# 
# polygon(c(1:n, n:1), c(allFreqCis[someIndex, , 2],
#                        rev(allFreqCis[someIndex, , 1])),
#         col=freqColours[2], border=freqColours[1], lwd=2,
#         density = NULL, angle = -20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval=FALSE---------------------------------------------------------------
# someA <- 0
# someG <- 1
# bayesCi <- matrix(nrow=n, ncol=2)
# 
# meanVector <- 1/(1:n)*cumsum(someDataSingle)
# 
# for (i in 1:n) {
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=i, meanObs=meanVector[i], parameter=NULL,
#     a=someA, g=someG, eType="credibleInterval")
#   bayesCi[i, ] <- tempResult
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# polygon(c(1:n, n:1), c(bayesCi[, 2], rev(bayesCi[, 1])),
#         col=bayesColours[2], border=bayesColours[1], lwd=2,
#         density = NULL, angle = 20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval = FALSE-------------------------------------------------------------
# # The indices where the lower bound is above muTrue
# which(bayesCi[, 1] > muTrue)
# 
# # No indices where the upper bound is below muTrue
# which(bayesCi[, 2] < muTrue)

## ----eval=FALSE---------------------------------------------------------------
# mIter <- 1000
# someA <- muTrue
# 
# set.seed(1)
# allData <- matrix(rnorm(mIter*n, mean=muTrue), nrow=mIter)
# allBayesCis <- array(dim=c(mIter, n, 2))
# 
# 
# # This indicates whether a simulation yielded an interval that
# # does not cover the true mean
# bayesCiError <- integer(mIter)
# 
# # This indicates the first time an interval does not cover the true mean
# firstPassageTime <- rep(Inf, times=mIter)
# 
# for (sim in 1:mIter) {
#   someData <- allData[sim, ]
#   meanVector <- 1/(1:n)*cumsum(someData)
# 
#   for (i in 1:n) {
#     tempResult <- computeConfidenceIntervalZ(
#       nEff=i, meanObs=meanVector[i],
#       a=someA, g=someG, parameter=NULL,
#       eType="credibleInterval")
#     allBayesCis[sim, i, ] <- tempResult
# 
#     if ((tempResult[1] > muTrue || tempResult[2] < muTrue)
#         && bayesCiError[sim] != 1) {
#       bayesCiError[sim] <- 1
#       firstPassageTime[sim] <- i
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# complementCoverageRate <- numeric(n)
# 
# for (i in 1:n) {
#   complementCoverageRate[i] <- mean(firstPassageTime <= i)
# }
# 
# bayesCoverageRate <- 1-complementCoverageRate
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n, 100*bayesCoverageRate, type="l", lwd=2, xlab="n",
#      ylab="Coverage rate (%)", col=bayesColours[1], ylim=c(60, 100))
# lines(c(1, n), 100*c(1-alpha, 1-alpha), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# failedIndeces <- which(bayesCiError==1)
# 
# someIndex <- failedIndeces[3]
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# 
# polygon(c(1:n, n:1), c(allBayesCis[someIndex, , 2],
#                        rev(allBayesCis[someIndex, , 1])),
#         col=bayesColours[2], border=bayesColours[1], lwd=2,
#         density = NULL, angle = 20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval=FALSE---------------------------------------------------------------
# designObj <- designSaviZ(meanDiffMin=0.5,
#                          testType="twoSample",
#                          sigma=sigma)
# designObj

## ----eval=FALSE---------------------------------------------------------------
# anytimeCi <- matrix(nrow=n, ncol=2)
# 
# meanVector <- 1/(1:n)*cumsum(someDataSingle)
# 
# for (i in 1:n) {
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=i, parameter=designObj$parameter,
#     meanObs=meanVector[i])
#   anytimeCi[i, ] <- tempResult
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# polygon(c(1:n, n:1), c(anytimeCi[, 2], rev(anytimeCi[, 1])),
#         col=eColours[2], border=eColours[1], lwd=2,
#         density = NULL, angle = -20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval = FALSE-------------------------------------------------------------
# # No indices where the lower bound is above muTrue
# which(anytimeCi[, 1] > muTrue)
# 
# # No indices where the upper bound is below muTrue
# which(anytimeCi[, 2] < muTrue)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# 
# polygon(c(1:n, n:1), c(anytimeCi[, 2], rev(anytimeCi[, 1])),
#         col=eColours[2], border=eColours[1], lwd=2,
#         density = NULL, angle = -20)
# polygon(c(1:n, n:1), c(freqCi[, 2], rev(freqCi[, 1])),
#         col=freqColours[2], border=freqColours[1], lwd=2,
#         density = 60, angle = -20)
# polygon(c(1:n, n:1), c(bayesCi[, 2], rev(bayesCi[, 1])),
#         col=bayesColours[2], border=bayesColours[1], lwd=2,
#         density = 40, angle = 20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval=FALSE---------------------------------------------------------------
# mIter <- 1000
# 
# set.seed(1)
# allData <- matrix(rnorm(mIter*n, mean=muTrue), nrow=mIter)
# allAnytimeCis <- array(dim=c(mIter, n, 2))
# 
# 
# # This indicates whether a simulation yielded an interval that
# # does not cover the true mean
# anytimeCiError <- integer(mIter)
# 
# # This indicates the first time an interval does not cover the true mean
# firstPassageTime <- rep(Inf, times=mIter)
# 
# for (sim in 1:mIter) {
#   someData <- allData[sim, ]
#   meanVector <- 1/(1:n)*cumsum(someData)
# 
#   for (i in 1:n) {
#     tempResult <- computeConfidenceIntervalZ(
#       nEff=i, meanObs=meanVector[i],
#       parameter=designObj$parameter)
# 
#     allAnytimeCis[sim, i, ] <- tempResult
# 
#     if ((tempResult[1] > muTrue || tempResult[2] < muTrue)
#         && anytimeCiError[sim] != 1) {
#       anytimeCiError[sim] <- 1
#       firstPassageTime[sim] <- i
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# complementCoverageRate <- numeric(n)
# 
# for (i in 1:n) {
#   complementCoverageRate[i] <- mean(firstPassageTime <= i)
# }
# 
# anytimeCiCoverageRate <- 1-complementCoverageRate
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n, 100*anytimeCiCoverageRate, type="l", lwd=2,
#      xlab="n", ylab="Coverage rate (%)", col=eColours[1],
#      ylim=c(60, 100))
# lines(c(1, n), 100*c(1-alpha, 1-alpha), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# failedIndeces <- which(anytimeCiError==1)
# 
# someIndex <- failedIndeces[3]
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(NULL, xlim=c(0, n), ylim=c(7, 9),
#      type="l", xlab = "", ylab = "", cex.lab = 1.3,
#      cex.axis = 1.3)
# 
# polygon(c(1:n, n:1), c(allAnytimeCis[someIndex, , 2],
#                        rev(allAnytimeCis[someIndex, , 1])),
#         col=eColours[2], border=eColours[1], lwd=2,
#         density = NULL, angle = 20)
# lines(c(1, n), c(muTrue, muTrue), lwd=2)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n, 100*anytimeCiCoverageRate, type="l", lwd=2,
#      xlab="n", ylab="Coverage rate (%)", col=eColours[1],
#      ylim=c(60, 100))
# lines(1:n, 100*bayesCoverageRate, col=bayesColours[1],
#       lwd=2)
# lines(1:n, 100*freqCoverageRate, col=freqColours[1], lwd=2)
# lines(c(1, n), 100*c(1-alpha, 1-alpha), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# someA <- 0
# 
# nDomain <- 1:30
# anytimeCiWidth <- credibleIntervalWidth <- freqCiWidth <- numeric(30)
# 
# for (i in nDomain) {
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=i, meanObs=8,  eType="freq",
#     parameter=NULL)
#   freqCiWidth[i] <- tempResult[2]-tempResult[1]
# 
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=i, meanObs=8, a=someA, parameter=NULL,
#     g=someG, eType="credibleInterval")
#   credibleIntervalWidth[i] <- tempResult[2]-tempResult[1]
# 
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=i, meanObs=8, parameter=designObj$parameter)
#   anytimeCiWidth[i] <- tempResult[2]-tempResult[1]
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(nDomain, anytimeCiWidth, col=eColours[1], type="l",
#      lwd=2, ylim=c(0, 12))
# lines(nDomain, credibleIntervalWidth, col=bayesColours[1],
#       lwd=2)
# lines(nDomain, freqCiWidth, col=freqColours[1], lwd=2,
#       lty=2)

## ----eval=FALSE---------------------------------------------------------------
# someA <- 0
# 
# gDomain <- seq(0.01, 100, by=0.01)
# anytimeCiWidth <- credibleIntervalWidth <- numeric(30)
# 
# 
# for (i in seq_along(gDomain)) {
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=1, meanObs=8, a=0, g=gDomain[i],
#     eType="credibleInterval", parameter=NULL)
# 
#   credibleIntervalWidth[i] <- tempResult[2]-tempResult[1]
# 
#   tempResult <- computeConfidenceIntervalZ(
#     nEff=1, meanObs=8, parameter=gDomain[i])
#   anytimeCiWidth[i] <- tempResult[2]-tempResult[1]
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(gDomain, anytimeCiWidth, col=eColours[1], type="l",
#      lwd=2, ylim=c(0, 16), xlab="g/gMom", ylab="Interval width", log="x")
# lines(gDomain, credibleIntervalWidth, col=bayesColours[1],
#       lwd=2)

