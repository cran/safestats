## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## -----------------------------------------------------------------------------
library("safestats")
eColours <- c("#1F78B4E6", "#A6CEE380")
lineColour <- "#DAA52066"
histInnerColour <- eColours[2]
histBorderColour <- eColours[1]

## ----eval=FALSE---------------------------------------------------------------
# alpha <- 0.05
# power <- 0.8
# meanDiffMin <- 10
# sigma <- 12
# 
# designObj <- designSaviZ(meanDiffMin=meanDiffMin, power=power,
#                          testType="twoSample",
#                          sigma=sigma, seed=1)
# designObj

## -----------------------------------------------------------------------------
nonLocalMomentDistribution <- function(delta, g) {
  delta^2/g*dnorm(delta, mean = 0, sd=sqrt(g))
}

deltaDomain <- seq(-2, 2, by=0.01)
deltaMin <- 10/12

momCurve <- nonLocalMomentDistribution(deltaDomain, g=deltaMin^2/2)

oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes()
plot(deltaDomain, momCurve, type="l", lwd=2, 
     xlab="Standardised effect size",
     ylab="Density")
lines(c(10/12, 10/12), c(0, 1), lty=3)
lines(-c(10/12, 10/12), c(0, 1), lty=3)

## ----eval=FALSE---------------------------------------------------------------
# set.seed(2)
# treatmentGroup <- rnorm(designObj$nPlan[1], mean=122, sd=sigma)
# controlGroup <- rnorm(designObj$nPlan[2], mean=112, sd=sigma)
# 
# resultObj <- saviZTest(x=treatmentGroup, y=controlGroup,
#                        designObj=designObj)
# resultObj

## ----eval=FALSE---------------------------------------------------------------
# result <- saviZTest(treatmentGroup, controlGroup[1:i],
#                     designObj=designObj, sequential=TRUE)
# plot(result)
# which(result$eValueVec > 1/alpha)

## ----eval=FALSE---------------------------------------------------------------
# plot(designObj, numSamplePaths=0)

## ----eval=FALSE---------------------------------------------------------------
# plot(designObj, wantQuantiles=c(0.5, 0.8))

## ----eval=FALSE---------------------------------------------------------------
# designLarger <- designSaviZ(meanDiffMin=1.2*designObj$esMin,
#                             sigma=sigma,
#                             parameter=designObj$parameter,
#                             eType=designObj$eType,
#                             testType=designObj$testType,
#                             nPlan=designObj$nPlan,
#                             seed=2)
# plot(designLarger, wantQuantiles=c(0.5, 0.8))

## ----eval=FALSE---------------------------------------------------------------
# designSmaller <- designSaviZ(designObj$esMin/1.2,
#                              sigma=sigma,
#                              parameter=designObj$parameter,
#                              eType=designObj$eType,
#                              testType=designObj$testType,
#                              nPlan=designObj$nPlan,
#                              seed=3)
# plot(designSmaller, wantQuantiles=c(0.5, 0.8))
# 
# nRejectedExperiments <- 1000-sum(designSmaller$breakVector)
# nRejectedExperiments

## ----eval=FALSE---------------------------------------------------------------
# designSaviZ(meanDiffMin=designObj$esMin/1.2, power=0.2,
#             parameter=designObj$parameter,
#             sigma=sigma, testType="twoSample", seed=4)

## ----eval=FALSE---------------------------------------------------------------
# nPlan2 <- 40
# nTotal <- designObj$nPlan[1]+nPlan2
# 
# notRejectedIndex <- which(designSmaller$breakVector==1)
# 
# firstPassageTimes <- nPlan2 <- 40
# nTotal <- designObj$nPlan[1]+nPlan2
# 
# notRejectedIndex <- which(designSmaller$breakVector==1)
# 
# firstPassageTimes <- designSmaller$bootObjPower$data
# 
# followUpStudy <- sampleStoppingTimesSaviZ(meanDiffTrue=designObj$esMin/1.2,
#                                           sigma=sigma,
#                                           parameter=designObj$parameter,
#                                           eType=designObj$eType,
#                                           testType=designObj$testType,
#                                           nSim=length(notRejectedIndex),
#                                           nMax=nPlan2, seed=5)
# 
# # This is just to make the indices of the original and follow-up align
# someMatrix <- matrix(nrow=1e3L, ncol=nPlan2)
# someMatrix[notRejectedIndex, ] <- followUpStudy$samplePaths
# 
# metaEVariablePaths <-
#   someMatrix*designSmaller$samplePaths[, designObj$nPlan[1]]
# 
# for (i in notRejectedIndex) {
#   firstPassageTimes[i] <-
#     suppressWarnings(
#       min(which(metaEVariablePaths[i, ] >= 20))+designObj$nPlan[1])
# }
# 
# eReject <- integer(designObj$nPlan[1]+nPlan2)
# 
# for (i in 1:(designObj$nPlan[1]+nPlan2)) {
#   eReject[i] <-
#     mean(firstPassageTimes <= i)
# }
# 
# oldPar <- setSafeStatsPlotOptionsAndReturnOldOnes();
# plot(1:nTotal, 100*eReject, type="l",
#      xlab="n", ylab="Correct rejections (%)", lwd=2,
#      col=eColours[1], ylim=c(0, 90))
# lines(c(1, nTotal), c(80, 80), lwd=2, lty=2)
# lines(c(designObj$nPlan[1], designObj$nPlan[1]),
#       c(0, 80), col="lightgrey", lty=2)
# 
# which(eReject >= 0.8)-designObj$nPlan[1]

