## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## ----eval=FALSE---------------------------------------------------------------
# library("safestats")
# # freqColours <- c("#E31A1CE6", "#FB9A9980")
# freqColours <- c("#FFDA1A", "#DAA52066")
# eColours <- c("#1F78B4E6", "#A6CEE380")

## ----eval=FALSE---------------------------------------------------------------
# n1 <- n2 <- 100
# sigmaTrue <- 12
# alpha <- 0.05
# 
# muGlobal <- 112
# 
# set.seed(2)
# dataGroup1 <- rnorm(n1, mean=muGlobal, sd=sigmaTrue)
# set.seed(3)
# dataGroup2 <- rnorm(n2, mean=muGlobal, sd=sigmaTrue)

## ----eval=FALSE---------------------------------------------------------------
# # # PSEUDO CODE
# # #
# # #     THIS CODE DOES NOT RUN
# # #
# # # Initiate
# # n <- 1
# # pValue <- 1
# #
# # while (pValue > alpha) {
# #   pValue <- computePValueZTest(x=x[1:n], y=y[1:n])
# #
# #   if (pValue < alpha) {
# #     "Reject the null"
# #     stop()
# #   } else {
# #     "Increase sample size and test again
# #           at the start the start of the while loop"
# #     n <- n + 1
# #   }
# # }

## ----eval=FALSE---------------------------------------------------------------
# pValueVector <- vector("numeric", length=n1)
# 
# for (i in 1:n1) {
#   pValueVector[i] <- pValueZTest(x=dataGroup1[1:i],
#                                  y=dataGroup2[1:i],
#                                  sigma=sigmaTrue)$pValue
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# 
# plot(1:n1, pValueVector, type="l", lwd=2, xlab="n",
#      ylab="p-value",
#      col=freqColours[1])
# abline(h=0.05, lty=2, lwd=2)

## ----eval=FALSE---------------------------------------------------------------
# mIter <- 1000
# 
# allData <- generateNormalData(c(n1, n2), muGlobal=muGlobal,
#                               nSim=mIter,
#                               meanDiffTrue=0, seed=1,
#                               sigmaTrue=sigmaTrue)
# allPValues <- matrix(nrow=mIter, ncol=n1)
# 
# # This indicates whether a simulation yielded a "significant" p-value
# pValueUnderAlpha <- vector("integer", length=mIter)
# 
# # This indicates the first time an experiment yielded a "significant" p-value
# # Default is Inf, which indicates that the p-value didn't dip below alpha
# firstPassageTime <- rep(Inf, times=mIter)
# 
# # Used to vectorise the computations for the the z-statistic
# n1Vector <- 1:n1
# n2Vector <- 1:n2
# nEffVector <- (1/n1Vector+1/n2Vector)^(-1)
# 
# 
# for (sim in 1:mIter) {
#   dataGroup1 <- allData$dataGroup1[sim, ]
#   dataGroup2 <- allData$dataGroup2[sim, ]
# 
#   x1BarVector <- 1/n1Vector*cumsum(dataGroup1)
#   x2BarVector <- 1/n2Vector*cumsum(dataGroup2)
#   zVector <- sqrt(nEffVector)*(x1BarVector - x2BarVector)/sigmaTrue
# 
#   for (i in 1:n1) {
#     currentPValue <- pValueFromZStat(zVector[i])
#     allPValues[sim, i] <- currentPValue
# 
#     if (currentPValue < alpha && pValueUnderAlpha[sim]!=1) {
#       pValueUnderAlpha[sim] <- 1
#       firstPassageTime[sim] <- i
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# numberOfDippingExperimentsAtTimeN <- integer(n1)
# 
# for (i in 1:n1) {
#   numberOfDippingExperimentsAtTimeN[i] <- sum(firstPassageTime <= i)
# }
# 
# pValueFalseRejects <-numberOfDippingExperimentsAtTimeN/mIter
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*pValueFalseRejects, type="l", xlab="n",
#      ylab="False positive rate (%)",
#      lwd=2, col=freqColours[1])
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# designObj <- designSaviZ(meanDiffMin=10,
#                          testType="twoSample",
#                          sigma=sigmaTrue)
# designObj

## ----eval=FALSE---------------------------------------------------------------
# # # PSEUDO CODE
# # #
# # #     THIS CODE DOES NOT RUN
# # #
# # # Initiate
# # n <- 1
# # eValue <- 1
# #
# # while (eValue < 1/alpha) {
# #   eValue <- saviZTest(x=x[1:n], y=y[1:n],
# #                       designObj=designObj)
# #
# #   if (eValue > 1/alpha) {
# #     "Reject the null"
# #     stop()
# #   } else {
# #     "Increase sample size and test again
# #           at the start of the while loop"
# #     n <- n + 1
# #   }
# # }

## ----eval=FALSE---------------------------------------------------------------
# # Single sample path data set as in the p-value example
# set.seed(2)
# dataGroup1 <- rnorm(n1, mean=muGlobal, sd=sigmaTrue)
# set.seed(3)
# dataGroup2 <- rnorm(n2, mean=muGlobal, sd=sigmaTrue)
# 
# result <- saviZTest(dataGroup1, dataGroup2, designObj=designObj)
# plot(result)

## ----eval=FALSE---------------------------------------------------------------
# mIter <- 1000
# 
# allData <- generateNormalData(c(n1, n2),
#                               muGlobal=muGlobal,
#                               nSim=mIter,
#                               meanDiffTrue=0, seed=1, sigmaTrue=sigmaTrue)
# allEValues <- matrix(nrow=mIter, ncol=n1)
# 
# # This indicates whether a simulation yielded e > 1/alpha
# eValueOver <- vector("integer", length=mIter)
# 
# # This indicates the first time an experiment yielded e > 1/alpha
# # Default is Inf, which indicates that the e didn't cross 1/alpha
# firstPassageTime <- rep(Inf, times=mIter)
# 
# # Used to vectorise the computations for the the z-statistic
# n1Vector <- 1:n1
# n2Vector <- 1:n2
# nEffVector <- (1/n1Vector+1/n2Vector)^(-1)
# 
# for (sim in 1:mIter) {
#   dataGroup1 <- allData$dataGroup1[sim, ]
#   dataGroup2 <- allData$dataGroup2[sim, ]
# 
#   x1BarVector <- 1/n1Vector*cumsum(dataGroup1)
#   x2BarVector <- 1/n2Vector*cumsum(dataGroup2)
#   zVector <- sqrt(nEffVector)*(x1BarVector - x2BarVector)/sigmaTrue
# 
#   for (i in 1:n1) {
#     currentEValue <- saviZTestStat(zVector[i],
#                                    parameter=designObj$parameter,
#                                    n1=n1Vector[i], n2=n2Vector[i],
#                                    sigma=sigmaTrue, eType=designObj$eType)$eValue
# 
#     allEValues[sim, i] <- currentEValue
# 
#     if (currentEValue > 1/alpha && eValueOver[sim]!=1) {
#       eValueOver[sim] <- 1
#       firstPassageTime[sim] <- i
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# numberOfCrossingExperimentsAtTimeN <- integer(n1)
# 
# for (i in 1:n1) {
#   numberOfCrossingExperimentsAtTimeN[i] <- sum(firstPassageTime <= i)
# }
# 
# eValueFalseRejects <- numberOfCrossingExperimentsAtTimeN/mIter
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*eValueFalseRejects, type="l",
#      xlab="n", ylab="False positive rate (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*pValueFalseRejects, type="l", xlab="n",
#      ylab="False positive rate (%)",
#      bty="n", lwd=2, ylim=c(0, 40), col=freqColours[1])
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)
# lines(1:n1, 100*eValueFalseRejects, col=eColours[1],
#       lwd=2)

