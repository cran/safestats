## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## ----install, eval=FALSE------------------------------------------------------
# install.packages("safestats")

## ----remotes, eval=FALSE------------------------------------------------------
# remotes::install_github("AlexanderLyNL/safestats", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(safestats)

## -----------------------------------------------------------------------------
freqColours <- c("#E31A1CE6", "#FB9A9980")
eColours <- c("#1F78B4E6", "#A6CEE380")
eColoursAlt <- c("#B15928E6", "#FFFF9980")

## -----------------------------------------------------------------------------
alpha <- 0.05
power <- 0.8
deltaMin <- 12/(sqrt(2)*15)
sigma <- 15

## ----echo = FALSE-------------------------------------------------------------
load("safeVignetteData/saviTDesignObj.RData")

## ----eval=FALSE---------------------------------------------------------------
# designObj <- designSaviT(deltaMin=deltaMin, alpha=alpha,
#                          power=power, sigma=sigma,
#                          alternative="greater",
#                          testType="paired", seed=1, pb=FALSE)

## -----------------------------------------------------------------------------
designObj

## ----eval = FALSE-------------------------------------------------------------
# designObj2 <- designSaviT(deltaMin=deltaMin, alpha=alpha,
#                           nPlan=c(30, 30), sigma=sigma,
#                           alternative="greater",
#                           testType="paired", seed=1, pb=FALSE)

## ----echo = FALSE-------------------------------------------------------------
load("safeVignetteData/saviTDesignObj2.RData")

## -----------------------------------------------------------------------------
designObj2

## ----eval = TRUE--------------------------------------------------------------
# Recall:
# alpha <- 0.05
# power <- 0.8
designObj3 <- designSaviT(nPlan=c(50, 50), 
                          alpha=alpha, power=power,
                          sigma=sigma,
                          alternative="greater",
                          testType="paired")
designObj3

## -----------------------------------------------------------------------------
set.seed(1)
preData <- rnorm(n=designObj$nPlan[1], mean=120, sd=15)
postData <- rnorm(n=designObj$nPlan[2], mean=120, sd=15)
# Thus, deltaTrue=0
saviTTest(x=preData, y=postData, 
          designObj=designObj, paired=TRUE)

## -----------------------------------------------------------------------------
savi.t.test(x=preData, y=postData, 
            designObj=designObj, paired=TRUE)

## ----echo=FALSE---------------------------------------------------------------
nSim <- 1000
load("safeVignetteData/eValuesTSimple.RData")

## ----eval=FALSE---------------------------------------------------------------
# # alpha <- 0.05
# nSim <- 1000
# 
# set.seed(1)
# eValues <- replicate(n=nSim, expr={
#   preData <- rnorm(n=designObj$nPlan[1], mean=120,
#                    sd=15)
#   postData <- rnorm(n=designObj$nPlan[2], mean=120,
#                     sd=15)
#   saviTTest(x=preData, y=postData,
#             designObj=designObj, paired=TRUE)$eValue}
# )

## -----------------------------------------------------------------------------
mean(eValues >= 20)
mean(eValues >= 20) <= alpha

## ----label=zMatrix------------------------------------------------------------
nSim <- 1000
muGlobal <- 120
n1 <- designObj$nPlan[1]

nullData <- generateNormalData(
  designObj$nPlan, muGlobal=muGlobal,
  nSim=nSim, deltaTrue=0, seed=1,
  sigma=sigma)

# Used to vectorise the computations of the t-statistic
n1Vector <- 1:n1
nuVector <- n1Vector-1

# Here we store all the t statistics across the 
# number of simulations (nSim) and time (n1)
tMatrix <- matrix(nrow=nSim, ncol=n1)

for (sim in 1:nSim) {
  dataGroup1 <- nullData$dataGroup1[sim, ]
  dataGroup2 <- nullData$dataGroup2[sim, ]
  
  differenceScore <- dataGroup1-dataGroup2
  
  # Vector of mean differences
  meanDiffVector <- 
    1/n1Vector*cumsum(differenceScore)
  
  # Vector of standard deviations
  sdMeanDiff <- sqrt(
    1/nuVector*(cumsum(differenceScore^2)-n1Vector*meanDiffVector^2)
  )
  
  tMatrix[sim, ] <- 
    sqrt(n1Vector)*meanDiffVector/sdMeanDiff
}

# The first t-value is undefined
tMatrix[, 1] <- 0

## ----label=pValuesFprCalculation----------------------------------------------
# Here we store all the p-values across the 
# number of simulations (nSim) and time (n1)
allPValues <- matrix(nrow=nSim, ncol=n1)

# Whenever this vector has a 1 it indicates that the simulate data 
# yielded a "significant" p-value, despite the data being generated under the null
pValueUnderAlpha <- vector("integer", length=nSim)

# This indicates the first time an experiment yielded a p-value < alpha=0.05
# Default is Inf, which indicates that the p-value dip below alpha
firstPassageTime <- rep(Inf, times=nSim)

for (sim in 1:nSim) {
  tVector <- tMatrix[sim, ]
  
  for (i in 2:n1) {
    currentPValue <- 1-stats::pt(tVector[i], df=nuVector[i])
    allPValues[sim, i] <- currentPValue
    
    if (currentPValue < alpha && 
        pValueUnderAlpha[sim]!=1) {
      pValueUnderAlpha[sim] <- 1
      firstPassageTime[sim] <- i
      break()
    }
  }
}
  
numberOfDippingExperimentsAtTimeN <- integer(n1)

for (i in 1:n1) {
  numberOfDippingExperimentsAtTimeN[i] <- 
    sum(firstPassageTime <= i)
}

pValueFalseRejects <- numberOfDippingExperimentsAtTimeN/nSim 

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*pValueFalseRejects, type="l", xlab="n",
#      ylab="Type I error (%)", ylim=c(0, 25),
#      lwd=2, col=freqColours[1])
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)

## ----echo=FALSE---------------------------------------------------------------
load("safeVignetteData/eValueFalseRejectsTSimple.RData")
load("safeVignetteData/eValueFalseRejectsT.RData")

## ----eval=FALSE---------------------------------------------------------------
# # Here we store all the e-values across the
# # number of simulations (nSim) and time (n1)
# eValues <- matrix(nrow=nSim, ncol=n1)
# 
# # This indicates whether a simulation yielded e >= 1/alpha
# eOver <- vector("integer", length=nSim)
# 
# # This indicates the first time an experiment yielded e >= 1/alpha
# # Default is Inf, which indicates that the e didn't cross 1/alpha
# firstPassageTimeE <- rep(Inf, times=nSim)
# 
# # This is the e-value at the end time, or whenever
# # it exceeds the threshold of 1/alpha
# eStopped <- numeric(nSim)
# 
# for (sim in 1:nSim) {
#   tVector <- tMatrix[sim, ]
# 
#   for (i in 1:n1) {
#     currentEValue <- saviTTestStat(
#       tVector[i], parameter=designObj$parameter,
#       n1=n1Vector[i], n2=n1Vector[i],
#       paired=TRUE,  eType=designObj$eType)$eValue
# 
#     eValues[sim, i] <- currentEValue
# 
#     if (currentEValue >= 1/alpha && eOver[sim]!=1) {
#       eOver[sim] <- 1
#       firstPassageTimeE[sim] <- i
#       eStopped[sim] <- currentEValue
#     }
# 
#     if (i==n1 && eOver[sim]!=1) {
#       eStopped[sim] <- currentEValue
#     }
#   }
# }
# 
# trackCrossing <- integer(n1)
# 
# for (i in 1:n1) {
#   trackCrossing[i] <-
#     sum(firstPassageTimeE <= i)
# }
# 
# eValueFalseRejects <- trackCrossing/nSim

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*eValueFalseRejects, type="l",
#      xlab="n", ylab="Type I error (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*pValueFalseRejects, type="l", xlab="n",
#      ylab="Type I error (%)",
#      lwd=2, col=freqColours[1], ylim=c(0, 25))
# lines(1:n1, 100*eValueFalseRejects, lwd=2,
#      col=eColours[1])
# lines(c(1, n1), c(5, 5), lwd=2, lty=2)

## ----echo = FALSE-------------------------------------------------------------
load("safeVignetteData/simDeltaTrueIsDeltaMin.RData")

## ----eval = FALSE-------------------------------------------------------------
# simDeltaTrueIsDeltaMin <-
#   sampleStoppingTimesSaviT(
#     deltaTrue=deltaMin, alternative="greater",
#     testType="paired", sigma=sigma,
#     nMax=designObj$nPlan, seed=1,
#     parameter=designObj$parameter,nSim=nSim)

## -----------------------------------------------------------------------------
mean(
  simDeltaTrueIsDeltaMin$eValuesStopped >=
    20)

## ----eval=FALSE---------------------------------------------------------------
# stoppingTimes <- simDeltaTrueIsDeltaMin$stoppingTimes
# 
# mean(stoppingTimes)
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes()
# hist(stoppingTimes,
#      breaks=min(stoppingTimes):max(stoppingTimes),
#      xlim=c(0, designObj$nPlanBatch[1]),col=eColours[2],
#      border=eColours[1], lwd=2, main="")

## ----eval=FALSE---------------------------------------------------------------
# firstPassageTimeAltEqual <- stoppingTimes
# firstPassageTimeAltEqual[
#   which(as.integer(simDeltaTrueIsDeltaMin$breakVector)==1)] <- Inf
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes()
# hist(firstPassageTimeAltEqual,
#      breaks=min(firstPassageTimeAltEqual):n1,
#      xlim=c(0, n1),col=eColours[2],
#      border=eColours[1], lwd=2, main="")

## ----eval=FALSE---------------------------------------------------------------
# trackCrossingAltEqual <- integer(n1)
# 
# for (i in 1:n1) {
#   trackCrossingAltEqual[i] <- sum(firstPassageTimeAltEqual <= i)
# }
# 
# eReject <- trackCrossingAltEqual/nSim
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*eReject, type="l",
#      xlab="n", ylab="Correct rejections (%)", lwd=2,
#      col=eColoursAlt[1], ylim=c(0, 80))
# lines(c(1, n1), c(80, 80), lwd=2, lty=2)

## ----echo = FALSE-------------------------------------------------------------
load("safeVignetteData/simDeltaTrueLargerDeltaMin.RData")

## ----eval = FALSE-------------------------------------------------------------
# simDeltaTrueLargerDeltaMin <-
#   sampleStoppingTimesSaviT(
#     deltaTrue=1.2*deltaMin, alternative="greater",
#     testType="paired", sigma=sigma,
#     nMax=designObj$nPlan, seed=1,
#     parameter=designObj$parameter,nSim=nSim)

## -----------------------------------------------------------------------------
mean(
  simDeltaTrueLargerDeltaMin$eValuesStopped >=
    20)

## ----eval=FALSE---------------------------------------------------------------
# stoppingTimes <-
#   simDeltaTrueLargerDeltaMin$stoppingTimes
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes()
# hist(stoppingTimes,
#      breaks=min(stoppingTimes):max(stoppingTimes),
#      xlim=c(0, designObj$nPlanBatch[1]),col=eColours[2],
#      border=eColours[1], lwd=2, main="")

## ----eval=FALSE---------------------------------------------------------------
# firstPassageTimeAltLarger <-
#   simDeltaTrueLargerDeltaMin$stoppingTimes
# firstPassageTimeAltLarger[
#   which(as.integer(simDeltaTrueLargerDeltaMin$breakVector)==1)] <- Inf
# 
# trackCrossingAltLarger <- integer(n1)
# 
# for (i in 1:n1) {
#   trackCrossingAltLarger[i] <- sum(firstPassageTimeAltLarger <= i)
# }
# 
# eReject <- trackCrossingAltLarger/nSim
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:n1, 100*eReject, type="l",
#      xlab="n", ylab="Correct rejections (%)", lwd=2,
#      col=eColoursAlt[1], ylim=c(0, 90))
# lines(c(1, n1), c(80, 80), lwd=2, lty=2)

## -----------------------------------------------------------------------------
tAtN1 <- numeric(nSim)

for (sim in 1:nSim) {
  dataGroup1 <- nullData$dataGroup1[sim, ]
  dataGroup2 <- nullData$dataGroup2[sim, ]
  
  meanDiff <- 
    mean(dataGroup1-dataGroup2)
  
  sdMeanDiff <- sd(dataGroup1-dataGroup2)
  
  tAtN1[sim] <- sqrt(n1)*meanDiff/sdMeanDiff
}

## -----------------------------------------------------------------------------
pValuesBatch1 <- numeric(nSim)

for (i in 1:nSim) {
  pValuesBatch1[i] <- 1-stats::pt(tAtN1[i], df=n1-1)
}

mean(pValuesBatch1 < alpha)

## -----------------------------------------------------------------------------
nullData2 <- generateNormalData(
  designObj$nPlan, muGlobal=muGlobal,
  nSim=nSim, deltaTrue=0, seed=2,
  sigma=sigma)

tAtN2 <- numeric(nSim)

for (sim in 1:nSim) {
  dataGroup1 <- c(nullData$dataGroup1[sim, ], 
                  nullData2$dataGroup1[sim, ])
  dataGroup2 <- c(nullData$dataGroup2[sim, ], 
                  nullData2$dataGroup2[sim, ])
  
  meanDiff <- mean(dataGroup1-dataGroup2)
  sdMeanDiff <- sd(dataGroup1-dataGroup2)
  
  tAtN2[sim] <- sqrt(2*n1)*meanDiff/sdMeanDiff
}

rejectedIndeces <- which(pValuesBatch1 < alpha)
notRejectedIndeces <- which(pValuesBatch1 >= alpha)

pValuesBatch2 <- numeric(nSim)
pValuesBatch2[rejectedIndeces] <- 
  pValuesBatch1[rejectedIndeces]

for (j in notRejectedIndeces) {
  pValuesBatch2[j] <- 1-stats::pt(tAtN2[j], df=2*n1-1)
}

mean(pValuesBatch2[notRejectedIndeces] < alpha)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# hist(pValuesBatch1, col=freqColours[1])
# abline(v=0.05)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# hist(pValuesBatch2[notRejectedIndeces], col=freqColours[2])
# abline(v=0.05)

## ----eval=FALSE---------------------------------------------------------------
# # Additional data under the null
# nullData3 <- generateNormalData(
#   9*designObj$nPlan, muGlobal=muGlobal,
#   nSim=nSim, deltaTrue=0, seed=3,
#   sigma=sigma)
# 
# # All the z statistics across the
# # number of simulations (nSim) and time (10*n1)
# tMatrixAll <- matrix(nrow=nSim, ncol=10*n1)
# 
# # Used to vectorise the computations for the the z-statistic
# n1Vector <- 1:(10*n1)
# nuVector <- n1Vector-1
# 
# for (sim in 1:nSim) {
#   dataGroup1 <- c(nullData$dataGroup1[sim, ],
#                   nullData3$dataGroup1[sim, ])
#   dataGroup2 <- c(nullData$dataGroup2[sim, ],
#                   nullData3$dataGroup2[sim, ])
# 
#   differenceScore <- dataGroup1-dataGroup2
# 
#   meanDiffVector <-
#     1/n1Vector*cumsum(differenceScore)
# 
#   # Vector of standard deviations
#   sdMeanDiff <- sqrt(
#     1/nuVector*(cumsum(differenceScore^2)-n1Vector*meanDiffVector^2)
#   )
# 
#   tMatrixAll[sim, ] <-
#     sqrt(n1Vector)*meanDiffVector/sdMeanDiff
# }
# 
# tMatrixAll[, 1] <- 0
# 
# # Here we store all the e-values across the
# # number of simulations (nSim) and time (n1)
# allEValues <- matrix(nrow=nSim, ncol=10*n1)
# allEValues[, 1:n1] <- eValues
# 
# eOverOptioCont <- eOver
# 
# for (sim in 1:nSim) {
#   tVector <- tMatrixAll[sim, ]
# 
#   for (i in (n1+1):(10*n1)) {
#     currentEValue <- saviTTestStat(
#       tVector[i], parameter=designObj$parameter,
#       n1=n1Vector[i], n2=n1Vector[i],
#       paired=TRUE,  sigma=sigma,
#       eType=designObj$eType)$eValue
# 
#     allEValues[sim, i] <- currentEValue
# 
#     if (currentEValue >= 1/alpha && eOverOptioCont[sim]!=1) {
#       eOverOptioCont[sim] <- 1
#       firstPassageTimeE[sim] <- i
#     }
#   }
# }
# 
# trackCrossingOptioCont <- integer(10*n1)
# 
# for (i in 1:(10*n1)) {
#   trackCrossingOptioCont[i] <-
#     sum(firstPassageTimeE <= i)
# }
# 
# eValueFalseRejects2 <-
#   trackCrossingOptioCont/nSim

## ----echo = FALSE-------------------------------------------------------------
load("safeVignetteData/eValueFalseRejects2T.RData")
# load("safeVignetteData/allEValuesTLarge.RData")

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(10*n1), 100*eValueFalseRejects2, type="l",
#      xlab="n", ylab="Type I error (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, 10*n1), c(5, 5), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# lowerQuartileLogEValueNull <- numeric(10*n1)
# medianLogEValueNull <- numeric(10*n1)
# upperQuartileLogEValueNull <- numeric(10*n1)
# 
# for (j in 1:(10*n1)) {
#   brie <- quantile(log(allEValues[, j]))
#   lowerQuartileLogEValueNull[j] <- brie[2]
#   medianLogEValueNull[j] <- brie[3]
#   upperQuartileLogEValueNull[j] <- brie[4]
# }
# 
# nDomain <- 1:(10*n1)
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(nDomain, medianLogEValueNull, col="black", lwd=2,
#      ylim=c(-6, 3), type="l", xlab="n",
#      ylab="log(eValues)")
# lines(nDomain, lowerQuartileLogEValueNull, col=eColours[1],
#       lwd=2, lty=1)
# lines(nDomain, upperQuartileLogEValueNull, col=eColours[1],
#       lwd=2, lty=1)
# lines(c(0, 10*n1), c(log(20), log(20)), lwd=2, col="grey", lty=2)

## ----eval=FALSE---------------------------------------------------------------
# # Data under the alternative
# altData <- generateNormalData(
#   10*designObj$nPlan, muGlobal=muGlobal,
#   nSim=nSim, deltaTrue=0.5, seed=2,
#   sigma=sigma)
# 
# # All the z statistics across the
# # number of simulations (nSim) and time (10*n1)
# tMatrixAllAlt <- matrix(nrow=nSim, ncol=10*n1)
# 
# # Used to vectorise the computations for the z-statistic
# n1Vector <- 1:(10*n1)
# 
# for (sim in 1:nSim) {
#   dataGroup1 <- altData$dataGroup1[sim, ]
#   dataGroup2 <- altData$dataGroup2[sim, ]
# 
#   meanDiffVector <-
#     1/n1Vector*cumsum(dataGroup1-dataGroup2)
# 
#   # The variance of the sum x + (-y) is the sum of the two variances
#   # Thus, 2*sigma^2
#   sdMeanDiff <- sqrt(2)*sigma
# 
#   tMatrixAllAlt[sim, ] <-
#     sqrt(n1Vector)*meanDiffVector/sdMeanDiff
# }
# 
# 
# # Here we store all the e-values across the
# # number of simulations (nSim) and time (n1)
# allEValuesAlt <- matrix(nrow=nSim, ncol=10*n1)
# 
# eOverAlt <- integer(nSim)
# firstPassageTimeEAlt <- rep(Inf, nSim)
# eStoppedAlt <- numeric(nSim)
# 
# for (sim in 1:nSim) {
#   tVector <- tMatrixAllAlt[sim, ]
# 
#   for (i in 1:(10*n1)) {
#     currentEValue <- saviZTestStat(
#       tVector[i], parameter=designObj$parameter,
#       n1=n1Vector[i], n2=n1Vector[i],
#       paired=TRUE,  sigma=sigma,
#       eType=designObj$eType)$eValue
# 
#     allEValuesAlt[sim, i] <- currentEValue
# 
#     if (currentEValue >= 1/alpha && eOverAlt[sim]!=1) {
#       eOverAlt[sim] <- 1
#       firstPassageTimeEAlt[sim] <- i
#       eStoppedAlt[sim] <- currentEValue
#     }
# 
#     if (i==n1 && eOverAlt[sim]!=1) {
#       eStoppedAlt[sim] <- currentEValue
#     }
#   }
# }
# 
# trackCrossingOptioCont <- integer(10*n1)
# 
# for (i in 1:(10*n1)) {
#   trackCrossingOptioCont[i] <-
#     sum(firstPassageTimeEAlt <= i)
# }
# 
# eValueCorrectRejects <-
#   trackCrossingOptioCont/nSim

## ----echo = FALSE-------------------------------------------------------------
# load("safeVignetteData/allEValuesAltT.RData")
load("safeVignetteData/eValueCorrectRejectsT.RData")

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(10*n1), 100*eValueCorrectRejects, type="l",
#      xlab="n", ylab="Correct rejections (%)", lwd=2,
#      col=eColoursAlt[1])#, #ylim=c(0, 5))
# lines(c(1, 10*n1), c(5, 5), lwd=2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
# lowerQuartileLogEValueAlt <- numeric(10*n1)
# medianLogEValueAlt <- numeric(10*n1)
# upperQuartileLogEValueAlt <- numeric(10*n1)
# 
# for (j in 1:(10*n1)) {
#   brie <- quantile(log(allEValuesAlt[, j]))
#   lowerQuartileLogEValueAlt[j] <- brie[2]
#   medianLogEValueAlt[j] <- brie[3]
#   upperQuartileLogEValueAlt[j] <- brie[4]
# }
# 
# nDomain <- 1:(10*n1)
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(nDomain, medianLogEValueAlt, col="red", lwd=2,
#      ylim=c(-6, 15), type="l", xlab="n",
#      ylab="log(eValues)")
# lines(c(0, 10*n1), c(log(20), log(20)), lwd=2, col="grey", lty=2)
# lines(nDomain, lowerQuartileLogEValueAlt, col=eColoursAlt[1],
#       lwd=2, lty=1)
# lines(nDomain, upperQuartileLogEValueAlt, col=eColoursAlt[1],
#       lwd=2, lty=1)
# 
# lines(nDomain, medianLogEValueNull, col="black", lwd=2)
# lines(nDomain, lowerQuartileLogEValueNull, col=eColours[1],
#       lwd=2, lty=1)
# lines(nDomain, upperQuartileLogEValueNull, col=eColours[1],
#       lwd=2, lty=1)

## ----eval=FALSE---------------------------------------------------------------
# rep2 <- selectivelyContinueZOrTTestData(
#   designObj, n1New=2*n1, testName="T-Test",
#   muGlobal=90, sigma=6, deltaTrue=0, nSim=nSim,
#   eValuesOld=eValues, eOverOld=eOver,
#   trackCrossingOld=trackCrossing,
#   firstPassageTimeOld=firstPassageTimeE,
#   eStoppedOld=eStopped, seed=2)

## ----echo = FALSE-------------------------------------------------------------
#load("safeVignetteData/allEValuesAltT.RData")
load("safeVignetteData/eValueCorrectRejectsT.RData")
load("safeVignetteData/rep2.RData")
load("safeVignetteData/rep3.RData")
load("safeVignetteData/rep4.RData")
# load("safeVignetteData/repAlt.RData")

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(3*n1), 100*rep2$trackCrossing/nSim, type="l",
#      xlab="n", ylab="Type I error (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, 3*n1), c(5, 5), lwd=2, lty=2)

## -----------------------------------------------------------------------------
rep2$extraRejections

## ----eval=FALSE---------------------------------------------------------------
# rep3 <- selectivelyContinueZOrTTestData(
#   designObj, n1New=ceiling(0.48*n1), testName="T-Test",
#   muGlobal=100, sigma=8, deltaTrue=0, nSim=1000,
#   eValuesOld=rep2$eValues, eOverOld=rep2$eOver,
#   trackCrossingOld=rep2$trackCrossing,
#   firstPassageTimeOld=rep2$firstPassageTime,
#   eStoppedOld=rep2$eStopped, seed=3)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(length(rep3$trackCrossing)), 100*rep3$trackCrossing/nSim, type="l",
#      xlab="n", ylab="Type I error (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, length(rep3$trackCrossing)), c(5, 5), lwd=2, lty=2)

## -----------------------------------------------------------------------------
rep3$extraRejections

## ----eval=FALSE---------------------------------------------------------------
# rep4 <- selectivelyContinueZOrTTestData(
#   designObj, n1New=ceiling(3.5*n1), testName="T-Test",
#   muGlobal=150, sigma=19, deltaTrue=0, nSim=1000,
#   eValuesOld=rep3$eValues, eOverOld=rep3$eOver,
#   trackCrossingOld=rep3$trackCrossing,
#   firstPassageTimeOld=rep3$firstPassageTime,
#   eStoppedOld=rep3$eStopped, seed=4)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(length(rep4$trackCrossing)), 100*rep4$trackCrossing/nSim, type="l",
#      xlab="n", ylab="Type I error (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 5))
# lines(c(1, length(rep4$trackCrossing)), c(5, 5), lwd=2, lty=2)

## -----------------------------------------------------------------------------
rep4$extraRejections

## ----eval=FALSE---------------------------------------------------------------
# totalN <- length(rep4$trackCrossing)
# 
# lowerQuartileLogEValueMetaNull <- numeric(totalN)
# medianLogEValueMetaNull <- numeric(totalN)
# upperQuartileLogEValueMetaNull <- numeric(totalN)
# 
# for (i in 1:totalN)  {
#   brie <- quantile(log(rep4$eValues[, i]))
# 
#   lowerQuartileLogEValueMetaNull[i] <- brie[2]
#   medianLogEValueMetaNull[i] <- brie[3]
#   upperQuartileLogEValueMetaNull[i] <- brie[4]
# }
# 
# yMin <- floor(min(lowerQuartileLogEValueMetaNull))
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:totalN, medianLogEValueMetaNull, col="black", lwd=2,
#      ylim=c(yMin, 3), type="l", xlab="n",
#      ylab="log(eValues)")
# lines(1:totalN, lowerQuartileLogEValueMetaNull, col=eColours[1],
#       lwd=2, lty=1)
# lines(1:totalN, upperQuartileLogEValueMetaNull, col=eColours[1],
#       lwd=2, lty=1)
# lines(c(0, totalN), c(log(20), log(20)), lwd=2, col="grey", lty=2)
# lines(c(n1, n1), c(yMin, 3), lty=2, col="lightgrey")
# lines(c(3*n1, 3*n1), c(yMin, 3), lty=2, col="lightgrey")
# lines(c(3.5*n1, 3.5*n1), c(yMin, 3), lty=2, col="lightgrey")

## ----eval=FALSE---------------------------------------------------------------
# repAlt <- selectivelyContinueZOrTTestData(
#   designObj, n1New=ceiling(2*n1), testName="T-Test",
#   muGlobal=145, sigma=15, deltaTrue=deltaMin, nSim=1000,
#   eValuesOld=simDeltaTrueIsDeltaMin$samplePaths,
#   eOverOld=!simDeltaTrueIsDeltaMin$breakVector,
#   trackCrossingOld=trackCrossingAltEqual,
#   firstPassageTimeOld=firstPassageTimeAltEqual,
#   eStoppedOld=simDeltaTrueIsDeltaMin$eValuesStopped, seed=6)

## ----eval=FALSE---------------------------------------------------------------
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:(length(repAlt$trackCrossing)), 100*repAlt$trackCrossing/nSim, type="l",
#      xlab="n", ylab="Correct rejections (%)", lwd=2,
#      col=eColoursAlt[1], ylim=c(0, 100))
# lines(c(1, length(repAlt$trackCrossing)), c(80, 80), lwd=2, lty=2)
# lines(c(n1, n1), c(0, 100), lty=2, col="lightgrey")

## -----------------------------------------------------------------------------
# safeDesignProportions <- designSaviTwoProportions(deltaMin=0.3, alpha=0.05,
#                                                   power=0.80, lowN=100,
#                                                   numberForSeed = 5227)

## -----------------------------------------------------------------------------
#safeDesignProportions$n.star

## -----------------------------------------------------------------------------
# sampleExample <- as.table(matrix(c(10, safeDesignProportions[["na"]]-10, 40,
#                                    safeDesignProportions[["nb"]]-40), 
#                                  byrow=TRUE, nrow=2))
# colnames(sampleExample) <- c(0, 1)
# sampleExample

## -----------------------------------------------------------------------------
#saviTwoProportionsTest(x = sampleExample, testDesign = safeDesignProportions)

## -----------------------------------------------------------------------------
# plotResult <- plotSaviTwoProportionsSampleSizeProfile(alpha=0.05,
#                                                       power=0.80,
#                                                       highN=200, 
#                                                       maxN=100,
#                                                       numberForSeed=5222)

## -----------------------------------------------------------------------------
# set.seed(5224)
# 
# optionalStoppingTrueMeanIsDesign <- 
#   simulateSpreadSampleSizeTwoProportions(
#     safeDesign=safeDesignProportions, M=1000,
#     parametersDataGeneratingDistribution=c(0.3, 0.6))
# 
# plotHistogramDistributionStoppingTimes(
#   optionalStoppingTrueMeanIsDesign, 
#   nPlan=safeDesignProportions[["n.star"]], 
#   deltaTrue = 0.3)

## -----------------------------------------------------------------------------
#power achieved:
#mean(optionalStoppingTrueMeanIsDesign$rejected == 1)

## -----------------------------------------------------------------------------
# set.seed(5224)
# 
# optionalStoppingTrueDifferenceBig <- 
#   simulateSpreadSampleSizeTwoProportions(
#     safeDesign=safeDesignProportions, M=1000, 
#     parametersDataGeneratingDistribution = c(0.2, 0.9))
# 
# plotHistogramDistributionStoppingTimes(
#   optionalStoppingTrueDifferenceBig, nPlan=safeDesignProportions[["n.star"]],
#   deltaTrue = 0.7)

## -----------------------------------------------------------------------------
#power achieved:
#mean(optionalStoppingTrueDifferenceBig$rejected == 1)

## -----------------------------------------------------------------------------
# set.seed(5224)
# 
# optionalStoppingTrueMeanNull <- 
#   simulateSpreadSampleSizeTwoProportions(
#     safeDesign=safeDesignProportions, M=1000, 
#     parametersDataGeneratingDistribution = c(0.5, 0.5))
# 
# plotHistogramDistributionStoppingTimes(
#   optionalStoppingTrueMeanNull, 
#   nPlan=safeDesignProportions[["n.star"]], 
#   deltaTrue = 0)

## -----------------------------------------------------------------------------
# The rate of false null rejections remained under alpha=0.05
#mean(optionalStoppingTrueMeanNull$rejected == 1)

## -----------------------------------------------------------------------------
# set.seed(5224)
# 
# fisher_result <- simulateFisherSpreadSampleSizeOptionalStopping(
#   deltaDesign=0.5, alpha=0.05, nDesign=safeDesignProportions$n.star, 
#   power=0.8, M=100, parametersDataGeneratingDistribution=c(0.5, 0.5))
# 
# mean(fisher_result$rejected == 1)

## -----------------------------------------------------------------------------
# notRejectedIndex <- which(optionalStoppingTrueMeanIsDesign$rejected==FALSE)
# eValuesNotRejected <- optionalStoppingTrueMeanIsDesign$s_values[notRejectedIndex]
# nullNotRejectedIndex <- which(optionalStoppingTrueMeanNull$rejected == FALSE)
# eValuesNotRejectedNull <- optionalStoppingTrueMeanNull$s_values[nullNotRejectedIndex]

## ----echo = FALSE-------------------------------------------------------------
# trueHist <- graphics::hist(x = eValuesNotRejected, plot = FALSE)
# nullHist <- graphics::hist(x = eValuesNotRejectedNull, plot = FALSE)
# yMax <- max(trueHist[["counts"]], nullHist[["counts"]])
# graphics::par(cex.main=1.5, mar=c(5, 6, 4, 4)+0.1, mgp=c(3.5, 1, 0), cex.lab=1.5,
#               font.lab=2, cex.axis=1.3, bty="n", las=1)
# graphics::plot(nullHist, xlim = c(0, max(eValuesNotRejected, eValuesNotRejectedNull)), 
#                freq = FALSE, col = "blue", density = 20, angle = 45, xlab = "e-values", 
#                main = "Histogram of e-values where null not rejected")
# graphics::plot(trueHist, add = TRUE, freq = FALSE, col = "red", density = 20, 
#                angle = -45)
# graphics::legend(x = "topright", legend = c("True delta: null", "True delta: design"), fill = c("blue", "red"))

## ----optionalContinuation2x2--------------------------------------------------
# continueIndex <- which(optionalStoppingTrueMeanIsDesign$s_values < 20 & 
#                          optionalStoppingTrueMeanIsDesign$s_values > 10)
# 
# interestingEValues <-
#   optionalStoppingTrueMeanIsDesign$s_values[continueIndex]
# 
# newEValues <- 
#   simulateOptionalContinuationTwoProportions(
#     interestingEValues, nFollowUp=40, 
#     parametersDataGeneratingDistribution=c(0.3, 0.6))
# 
# mean(newEValues>=20)

## ----optionalContinuation2x2Null----------------------------------------------
# continueIndex <- optionalStoppingTrueMeanNull$s_values < 20 & 
#   optionalStoppingTrueMeanNull$s_values > 1
# 
# interestingEValues <-optionalStoppingTrueMeanNull$s_values[continueIndex]
# 
# newEValues <- 
#   simulateOptionalContinuationTwoProportions(
#     interestingEValues, nFollowUp=40, 
#     parametersDataGeneratingDistribution=c(0.5, 0.5))
# 
# mean(newEValues>=20)

## -----------------------------------------------------------------------------
# safeDesignProportionsOneSided <- 
#   designSaviTwoProportions(deltaMin=0.5, alternative="greater",
#                            numberForSeed = 291202)

## -----------------------------------------------------------------------------
# sampleExampleGreater <- 
#   as.table(matrix(c(5, safeDesignProportionsOneSided[["na"]]-5, 19,
#                     safeDesignProportionsOneSided[["nb"]]-19), 
#                   byrow=TRUE, nrow=2))
# 
# colnames(sampleExampleGreater) <- c(0,1)
# sampleExampleGreater

## -----------------------------------------------------------------------------
# saviTwoProportionsTest(x=sampleExampleGreater, 
#                        testDesign=safeDesignProportionsOneSided)

## -----------------------------------------------------------------------------
# sampleExampleLesser <- 
#   as.table(matrix(c(safeDesignProportionsOneSided[["na"]]-5, 5,
#                     safeDesignProportionsOneSided[["nb"]]-19, 19), 
#                   byrow=TRUE, nrow=2))
# 
# colnames(sampleExampleGreater) <- colnames(sampleExampleLesser) <- c(0,1)
# sampleExampleLesser

## -----------------------------------------------------------------------------
# saviTwoProportionsTest(x=sampleExampleLesser,
#                        testDesign=safeDesignProportionsOneSided)

## -----------------------------------------------------------------------------
# safeDesignProportionsImbalanced <- 
#   designSaviTwoProportions(deltaMin=0.3, alpha=0.05, power=0.80, lowN=120,
#                            sampleSizeRatio=2)
# safeDesignProportionsImbalanced

