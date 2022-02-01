## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("safestats")

## ----remotes, eval=FALSE------------------------------------------------------
#  remotes::install_github("AlexanderLyNL/safestats", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(safestats)

## -----------------------------------------------------------------------------
alpha <- 0.05
beta <- 0.2
deltaMin <- 9/(sqrt(2)*15)

## ---- echo = FALSE------------------------------------------------------------
# designObj <- designSafeT(deltaMin=deltaMin, alpha=alpha, beta=beta,
#                          alternative="greater", testType="paired", seed=1, pb=FALSE)
# save(designObj, file="safeTVignetteData/safeTDesignObject.Rdata")
load("safeTVignetteData/safeTDesignObject.Rdata")

## ---- eval=FALSE--------------------------------------------------------------
#  designObj <- designSafeT(deltaMin=deltaMin, alpha=alpha, beta=beta,
#                           alternative="greater", testType="paired", seed=1, pb=FALSE)

## -----------------------------------------------------------------------------
designObj

## ---- echo = FALSE------------------------------------------------------------
# plotSafeTSampleSizeProfile <- plotSafeTDesignSampleSizeProfile(alpha=alpha, beta=beta,
#                                                                lowDeltaMin=0.1, highDeltaMin=1,
#                                                                nMax=100, seed=1,
#                                                                alternative="greater",
#                                                                testType="paired", nSim=1000,
#                                                                pb=FALSE)
# save(plotSafeTSampleSizeProfile, file="safeTVignetteData/plotSafeTSampleSizeProfile.Rdata")
load("safeTVignetteData/plotSafeTSampleSizeProfile.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  # Recall:
#  # alpha <- 0.05
#  # beta <- 0.2
#  plotSafeTSampleSizeProfile <- plotSafeTDesignSampleSizeProfile(alpha=alpha, beta=beta,
#                                             lowDeltaMin=0.1, highDeltaMin=1,
#                                             nMax=100, seed=1, alternative="greater",
#                                             testType="paired", nSim=1000, pb=FALSE)

## -----------------------------------------------------------------------------
plotSafeTSampleSizeProfile$deltaDomain
plotSafeTSampleSizeProfile$allN1PlanSafe

## -----------------------------------------------------------------------------
set.seed(1)
preData <- rnorm(n=designObj$nPlan[1], mean=120, sd=15)
postData <- rnorm(n=designObj$nPlan[2], mean=120, sd=15)
# Thus, the true delta is 0:
# deltaTrue <- (120-120)/(sqrt(2)*15)
safeTTest(x=preData, y=postData, alternative = "greater",
          designObj=designObj, paired=TRUE)

## -----------------------------------------------------------------------------
safe.t.test(x=preData, y=postData, alternative = "greater",
            designObj=designObj, paired=TRUE)

## -----------------------------------------------------------------------------
# alpha <- 0.05

set.seed(1)
eValues <- replicate(n=1000, expr={
  preData <- rnorm(n=designObj$nPlan[1], mean=120, sd=15)
  postData <- rnorm(n=designObj$nPlan[2], mean=120, sd=15)
  safeTTest(x=preData, y=postData, alternative = "greater",
            designObj=designObj, paired=TRUE)$eValue}
)

mean(eValues > 20)
mean(eValues > 20) < alpha

## ---- echo=FALSE--------------------------------------------------------------
nSim <- 500L

## -----------------------------------------------------------------------------
# Recall:
# alpha <- 0.05
# beta <- 0.2

freqDesignObj <- designFreqT(deltaMin=deltaMin, alpha=alpha, beta=beta,
                             alternative="greater", testType="paired")

## ---- echo = FALSE------------------------------------------------------------
# nSim <- 500L
# simResultDeltaTrueIsZero <- simulate(object=designObj, nSim=nSim, seed=1,
#                                      deltaTrue=0, freqOptioStop=TRUE,
#                                      nPlanFreq=freqDesignObj$nPlan,
#                                      muGlobal=120, sigmaTrue=15, pb=FALSE)
# simResultDeltaTrueIsZero$bootObjN1Plan <- NULL
# save(simResultDeltaTrueIsZero, file="safeTVignetteData/simResultDeltaTrueIsZero.Rdata")
load("safeTVignetteData/simResultDeltaTrueIsZero.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  nSim <- 500
#  simResultDeltaTrueIsZero <- simulate(object=designObj, nSim=nSim, seed=1,
#                                       deltaTrue=0, freqOptioStop=TRUE,
#                                       nPlanFreq=freqDesignObj$nPlan,
#                                       muGlobal=120, sigmaTrue=15, pb=FALSE)

## -----------------------------------------------------------------------------
simResultDeltaTrueIsZero

## -----------------------------------------------------------------------------
# Recall:
# alpha <- 0.05
# beta <- 0.2
deltaMin <- 9/(sqrt(2)*15)      # = 0.42

## ---- echo = FALSE------------------------------------------------------------
# simResultDeltaTrueIsDeltaMin <- simulate(object=designObj, nSim=nSim,
#                                          seed=1, deltaTrue=deltaMin,
#                                          muGlobal=120, sigmaTrue=15, pb=FALSE)
# save(simResultDeltaTrueIsDeltaMin, file="safeTVignetteData/simResultDeltaTrueIsDeltaMin.Rdata")
load("safeTVignetteData/simResultDeltaTrueIsDeltaMin.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  simResultDeltaTrueIsDeltaMin <- simulate(object=designObj, nSim=nSim,
#                                           seed=1, deltaTrue=deltaMin,
#                                           muGlobal=120, sigmaTrue=15, pb=FALSE)

## -----------------------------------------------------------------------------
simResultDeltaTrueIsDeltaMin

## -----------------------------------------------------------------------------
plot(simResultDeltaTrueIsDeltaMin)
tabResult <- table(simResultDeltaTrueIsDeltaMin$safeSim$allN)
tabResult

## -----------------------------------------------------------------------------
plot(simResultDeltaTrueIsDeltaMin, showOnlyNRejected=TRUE)

## -----------------------------------------------------------------------------
# Recall:
# alpha <- 0.05
# beta <- 0.2
# deltaMin <- 9/(sqrt(2)*15)      # = 0.42
deltaTrueLarger <- 0.6

## ---- echo = FALSE------------------------------------------------------------
# simResultDeltaTrueLargerThanDeltaMin <- simulate(object=designObj,
#                                                  nSim=nSim, seed=1,
#                                                  deltaTrue=deltaTrueLarger,
#                                                  muGlobal=120, sigmaTrue=15, pb=FALSE)
# save(simResultDeltaTrueLargerThanDeltaMin, file="safeTVignetteData/simResultDeltaTrueLargerThanDeltaMin.Rdata")
load("safeTVignetteData/simResultDeltaTrueLargerThanDeltaMin.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  simResultDeltaTrueLargerThanDeltaMin <- simulate(object=designObj,
#                                                   nSim=nSim, seed=1,
#                                                   deltaTrue=deltaTrueLarger,
#                                                   muGlobal=120, sigmaTrue=15, pb=FALSE)

## -----------------------------------------------------------------------------
simResultDeltaTrueLargerThanDeltaMin

## -----------------------------------------------------------------------------
plot(simResultDeltaTrueLargerThanDeltaMin)

## -----------------------------------------------------------------------------
dataBatch1 <- generateNormalData(nPlan=freqDesignObj$nPlan,
                               deltaTrue=0, nSim=nSim, paired=TRUE, seed=1,
                               muGlobal=120, sigmaTrue=15)

pValuesBatch1 <- vector("numeric", length=nSim)

for (i in seq_along(pValuesBatch1)) {
  pValuesBatch1[i] <- t.test(x=dataBatch1$dataGroup1[i, ], 
                             y=dataBatch1$dataGroup2[i, ], 
                             alternative="greater", paired=TRUE)$p.value
}
mean(pValuesBatch1 > alpha)
sum(pValuesBatch1 < alpha)

## -----------------------------------------------------------------------------
selectivelyContinueDeltaTrueIsZeroWithP <-
  selectivelyContinueTTestCombineData(oldValues=pValuesBatch1,
                                      valuesType="pValues", 
                                      alternative="greater", 
                                      oldData=dataBatch1,
                                      deltaTrue=0,
                                      n1Extra=freqDesignObj$nPlan[1],
                                      n2Extra=freqDesignObj$nPlan[2],
                                      alpha=alpha,
                                      seed=2, paired=TRUE,
                                      muGlobal=120, sigmaTrue=15)

## -----------------------------------------------------------------------------
pValuesBatch1To2 <- selectivelyContinueDeltaTrueIsZeroWithP$newValues
sum(pValuesBatch1To2 < alpha)

## -----------------------------------------------------------------------------
dataBatch1 <- list(dataGroup1=simResultDeltaTrueIsZero$safeSim$dataGroup1,
                   dataGroup2=simResultDeltaTrueIsZero$safeSim$dataGroup2)

eValuesBatch1 <- simResultDeltaTrueIsZero$safeSim$eValues
sum(eValuesBatch1 > 1/alpha)

## -----------------------------------------------------------------------------
selectivelyContinueDeltaTrueIsZero <- 
  selectivelyContinueTTestCombineData(oldValues=eValuesBatch1,
                                      designObj=designObj,
                                      alternative="greater", 
                                      oldData=dataBatch1,
                                      deltaTrue=0,
                                      seed=2, paired=TRUE,
                                      muGlobal=120, sigmaTrue=15,
                                      moreMainText="Batch 1-2")

## -----------------------------------------------------------------------------
eValuesBatch1To2 <- selectivelyContinueDeltaTrueIsZero$newValues
sum(eValuesBatch1To2 > 1/alpha)
length(eValuesBatch1To2)

## -----------------------------------------------------------------------------
eValuesBatch1To2 <- selectivelyContinueDeltaTrueIsZero$newValue
dataBatch1To2 <- selectivelyContinueDeltaTrueIsZero$combinedData

selectivelyContinueDeltaTrueIsZero <- 
  selectivelyContinueTTestCombineData(oldValues=eValuesBatch1To2,
                                      designObj=designObj,
                                      alternative="greater", 
                                      oldData=dataBatch1To2,
                                      deltaTrue=0,
                                      seed=3, paired=TRUE, 
                                      muGlobal=120, sigmaTrue=15,
                                      moreMainText=paste("Batch: 1 to", 3))
sum(selectivelyContinueDeltaTrueIsZero$newValues > 1/alpha)

## ---- echo = FALSE------------------------------------------------------------
# simResultDeltaTrueLessThanDeltaMin <- simulate(object=designObj, nSim=nSim,
#                                                seed=1, deltaTrue=0.3,
#                                                muGlobal=120, sigmaTrue=15, pb=FALSE)
# save(simResultDeltaTrueLessThanDeltaMin, file="safeTVignetteData/simResultDeltaTrueLessThanDeltaMin.Rdata")
load("safeTVignetteData/simResultDeltaTrueLessThanDeltaMin.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  simResultDeltaTrueLessThanDeltaMin <- simulate(object=designObj, nSim=1000L,
#                                                 seed=1, deltaTrue=0.3,
#                                                 muGlobal=120, sigmaTrue=15, pb=FALSE)

## -----------------------------------------------------------------------------
simResultDeltaTrueLessThanDeltaMin

## -----------------------------------------------------------------------------
dataBatch1 <- list(
  dataGroup1=simResultDeltaTrueLessThanDeltaMin$safeSim$dataGroup1,
  dataGroup2=simResultDeltaTrueLessThanDeltaMin$safeSim$dataGroup2
)

eValuesBatch1 <- simResultDeltaTrueLessThanDeltaMin$safeSim$eValues
sum(eValuesBatch1 > 1/alpha)

## -----------------------------------------------------------------------------
selectivelyContinueDeltaTrueLessThanDeltaMin <- 
  selectivelyContinueTTestCombineData(oldValues=eValuesBatch1,
                                      designObj=designObj,
                                      alternative="greater", 
                                      oldData=dataBatch1,
                                      deltaTrue=deltaMin,
                                      seed=2, paired=TRUE, muGlobal=120,
                                      sigmaTrue=15)

## -----------------------------------------------------------------------------
eValuesBatch1To2 <- selectivelyContinueDeltaTrueLessThanDeltaMin$newValues
sum(eValuesBatch1To2 > 1/alpha)

## -----------------------------------------------------------------------------
eValuesOri <- simResultDeltaTrueIsZero$safeSim$eValues

## -----------------------------------------------------------------------------
# Needs to be larger than 1/designObj$n1Plan to have at least two samples 
# in the replication attempt
someConstant <- 1.2

repData <- generateNormalData(nPlan=c(ceiling(someConstant*designObj$nPlan[1]),
                                     ceiling(someConstant*designObj$nPlan[2])),
                             deltaTrue=0, nSim=nSim, 
                             muGlobal=90, sigmaTrue=6,
                             paired=TRUE, seed=2)

eValuesRep <- vector("numeric", length=nSim)

for (i in seq_along(eValuesRep)) {
  eValuesRep[i] <- safeTTest(x=repData$dataGroup1[i, ], 
                          y=repData$dataGroup2[i, ], 
                          designObj=designObj,
                          alternative="greater", paired=TRUE)$eValue
}
eValuesMultiply <- eValuesOri*eValuesRep
mean(eValuesMultiply > 1/alpha)

## -----------------------------------------------------------------------------
eValuesOri <- simResultDeltaTrueIsDeltaMin$safeSim$eValues

## -----------------------------------------------------------------------------
someConstant <- 1.2

repData <- generateNormalData(nPlan=c(ceiling(someConstant*designObj$nPlan[1]),
                                     ceiling(someConstant*designObj$nPlan[2])), 
                             deltaTrue=deltaMin, nSim=nSim, 
                             muGlobal=110, sigmaTrue=50,
                             paired=TRUE, seed=2)

eValuesRep <- vector("numeric", length=nSim)

for (i in seq_along(eValuesRep)) {
  eValuesRep[i] <- safeTTest(x=repData$dataGroup1[i, ], 
                          y=repData$dataGroup2[i, ], 
                          designObj=designObj,
                          alternative="greater", paired=TRUE)$eValue
}
eValuesMulti <- eValuesOri*eValuesRep
mean(eValuesMulti > 1/alpha)

## -----------------------------------------------------------------------------
# safeDesignProportions <- designSafeTwoProportions(deltaMin=0.3, alpha=0.05,
#                                                   beta=0.20, lowN=100,
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
#safeTwoProportionsTest(x = sampleExample, testDesign = safeDesignProportions)

## -----------------------------------------------------------------------------
# plotResult <- plotSafeTwoProportionsSampleSizeProfile(alpha=0.05,
#                                                       beta=0.20,
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

## ---- echo = FALSE------------------------------------------------------------
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
#   designSafeTwoProportions(deltaMin=0.5, alternative="greater",
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
# safeTwoProportionsTest(x=sampleExampleGreater, 
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
# safeTwoProportionsTest(x=sampleExampleLesser,
#                        testDesign=safeDesignProportionsOneSided)

## -----------------------------------------------------------------------------
# safeDesignProportionsImbalanced <- 
#   designSafeTwoProportions(deltaMin=0.3, alpha=0.05, beta=0.20, lowN=120,
#                            sampleSizeRatio=2)
# safeDesignProportionsImbalanced

