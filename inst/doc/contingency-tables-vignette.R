## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8
)

library(safestats)

## -----------------------------------------------------------------------------
ya <- c(1,1,1,1,1,1,0,1,1,1)
yb <- c(0,0,1,0,1,0,0,0,0,0)

## ---- echo = FALSE------------------------------------------------------------
set.seed(1082021)
# balancedSafeDesign <- designSafeTwoProportions(
#   na = 1,
#   nb = 1,
#   alpha = 0.05, #significance level for testing
#   beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
#   delta = 0.3
# )
# save(balancedSafeDesign, file="safe2x2VignetteData/safe2x2DesignObject.Rdata")
load("safe2x2VignetteData/safe2x2DesignObject.Rdata")

## ---- eval=FALSE--------------------------------------------------------------
#  balancedSafeDesign <- designSafeTwoProportions(
#    na = 1,
#    nb = 1,
#    alpha = 0.05, #significance level for testing
#    beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
#    delta = 0.3
#  )

## -----------------------------------------------------------------------------
print(balancedSafeDesign)

## -----------------------------------------------------------------------------
set.seed(19012022)
successProbabilityA <- 0.2
successProbabilityB <- 0.5
ya <- rbinom(n = balancedSafeDesign[["nPlan"]]["nBlocksPlan"], size = 1, prob = successProbabilityA)
yb <- rbinom(n = balancedSafeDesign[["nPlan"]]["nBlocksPlan"], size = 1, prob = successProbabilityB)

graphics::plot(x = seq_along(ya), y = cumsum(ya), type = "p", ylim = c(0, max(c(sum(ya), sum(yb)))),
     xlab = "block number", ylab = "# successes",
     main = "Total # successes per group over time")
graphics::points(x = seq_along(yb), y = cumsum(yb), type = "p", col = "grey")
graphics::legend(x = "topleft", legend = c("A", "B"), col = c("black", "grey"), pch = 1)

## -----------------------------------------------------------------------------
safe.prop.test(ya = ya, yb = yb, designObj = balancedSafeDesign)

## ---- echo = FALSE------------------------------------------------------------
# #make the plot voor standard parameter settings
# standardPrior <- list(betaA1 = 0.18,
#                       betaA2 = 0.18,
#                       betaB1 = 0.18,
#                       betaB2 = 0.18)
# #set a seed for the simulations
# set.seed(1082021)
# sampleSizePerMinimalDifference <- simulateTwoProportions(list(standard = standardPrior),
#                                            alternativeRestriction = "none",
#                                            alpha = 0.05,
#                                            beta = 0.2,
#                                            na = 1,
#                                            nb = 1,
#                                            deltamin = 0.3,
#                                            deltamax = 0.9
#                                          )
# save(sampleSizePerMinimalDifference, file = "safe2x2VignetteData/safe2x2SimObject.Rdata")
load("safe2x2VignetteData/safe2x2SimObject.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  #make the plot voor standard parameter settings
#  standardPrior <- list(betaA1 = 0.18,
#                        betaA2 = 0.18,
#                        betaB1 = 0.18,
#                        betaB2 = 0.18)
#  #set a seed for the simulations
#  set.seed(1082021)
#  sampleSizePerMinimalDifference <- simulateTwoProportions(list(standard = standardPrior),
#                                             alternativeRestriction = "none",
#                                             alpha = 0.05,
#                                             beta = 0.2,
#                                             na = 1,
#                                             nb = 1,
#                                             deltamin = 0.3,
#                                             deltamax = 0.9
#                                           )

## ---- echo = FALSE------------------------------------------------------------
someIndex <- max(which(sampleSizePerMinimalDifference$simdata$worstCaseQuantile <= 50))
minDetectableEffect <- round(sampleSizePerMinimalDifference$simdata$delta[someIndex], 2)

## -----------------------------------------------------------------------------
graphics::plot(sampleSizePerMinimalDifference)

## ---- echo = FALSE------------------------------------------------------------
# unbalancedSafeDesign <- designSafeTwoProportions(
#   na = 2,
#   nb = 1,
#   alpha = 0.05,
#   beta = 0.2,
#   delta = 0.3
# )
# save(unbalancedSafeDesign, file = "safe2x2VignetteData/safe2x2UnbalancedDesignObject.Rdata")
load("safe2x2VignetteData/safe2x2UnbalancedDesignObject.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  unbalancedSafeDesign <- designSafeTwoProportions(
#    na = 2,
#    nb = 1,
#    alpha = 0.05,
#    beta = 0.2,
#    delta = 0.3
#  )

## -----------------------------------------------------------------------------
set.seed(692021)
yaUnbalanced <- rbinom(n = unbalancedSafeDesign[["nPlan"]]["nBlocksPlan"], size = 2, prob = successProbabilityA)
ybUnbalanced <- rbinom(n = unbalancedSafeDesign[["nPlan"]]["nBlocksPlan"], size = 1, prob = successProbabilityB)

print(yaUnbalanced)
print(ybUnbalanced)

## -----------------------------------------------------------------------------
safeTwoProportionsTest(ya = yaUnbalanced, yb = ybUnbalanced, designObj = unbalancedSafeDesign)

## -----------------------------------------------------------------------------
print(unbalancedSafeDesign)

## ---- echo = FALSE------------------------------------------------------------
# #make the plot voor standard parameter settings
# standardPrior <- unbalancedSafeDesign[["betaPriorParameterValues"]]
# uniformLikePrior <- list(betaA1 = 2, # pretend we "have seen" 2 success and
#                       betaA2 = 2,# 2 failure in group A before starting
#                       betaB1 = 1, # and 1 success and failure in group B
#                       betaB2 = 1)
# #set a seed for the simulations
# set.seed(1082021)
# sampleSizePerMinimalDifference <- simulateTwoProportions(
#    hyperparameterList = list(standard = standardPrior, uniform = uniformLikePrior),
#    alternativeRestriction = "none",
#    alpha = 0.05,
#    beta = 0.2,
#    na = 2,
#    nb = 1,
#    deltamin = 0.3,
#    deltamax = 0.9
#   )
# save(sampleSizePerMinimalDifference, file = "safe2x2VignetteData/safe2x2UnbalancedSimObject.Rdata")
load("safe2x2VignetteData/safe2x2UnbalancedSimObject.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  #make the plot voor standard parameter settings
#  standardPrior <- unbalancedSafeDesign[["betaPriorParameterValues"]]
#  uniformLikePrior <- list(betaA1 = 2, # pretend we "have seen" 2 success and
#                        betaA2 = 2,# 2 failure in group A before starting
#                        betaB1 = 1, # and 1 success and failure in group B
#                        betaB2 = 1)
#  #set a seed for the simulations
#  set.seed(1082021)
#  sampleSizePerMinimalDifference <- simulateTwoProportions(
#     hyperparameterList = list(standard = standardPrior, uniform = uniformLikePrior),
#     alternativeRestriction = "none",
#     alpha = 0.05,
#     beta = 0.2,
#     na = 2,
#     nb = 1,
#     deltamin = 0.3,
#     deltamax = 0.9
#    )

## -----------------------------------------------------------------------------
graphics::plot(sampleSizePerMinimalDifference)

## ---- echo = FALSE------------------------------------------------------------
# differenceRestrictedSafeDesign <- designSafeTwoProportions(
#   na = 1,
#   nb = 1,
#   alpha = 0.05, #significance level for testing
#   beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
#   delta = 0.3,
#   alternativeRestriction = "difference" #also available: logOddsRatio
# )
# save(differenceRestrictedSafeDesign, file = "safe2x2VignetteData/safe2x2DiffRestrictedDesign.Rdata")
load("safe2x2VignetteData/safe2x2DiffRestrictedDesign.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  differenceRestrictedSafeDesign <- designSafeTwoProportions(
#    na = 1,
#    nb = 1,
#    alpha = 0.05, #significance level for testing
#    beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
#    delta = 0.3,
#    alternativeRestriction = "difference" #also available: logOddsRatio
#  )

## -----------------------------------------------------------------------------
safe.prop.test(ya = ya, yb = yb, designObj = differenceRestrictedSafeDesign)

## -----------------------------------------------------------------------------
#set a seed for the simulations
set.seed(1082021)
optionalStoppingResult <- simulateOptionalStoppingScenarioTwoProportions(
    safeDesign = balancedSafeDesign,
    M = 1e3,
    thetaA = 0.2,
    thetaB = 0.5
)

## -----------------------------------------------------------------------------
plotHistogramDistributionStoppingTimes(safeSim = optionalStoppingResult, 
                                       nPlan = balancedSafeDesign[["nPlan"]]["nBlocksPlan"],
                                       deltaTrue = 0.3)

## -----------------------------------------------------------------------------
optionalStoppingResult[["powerOptioStop"]]

## -----------------------------------------------------------------------------
#set a seed for the simulations
set.seed(1082021)
optionalStoppingResultTrueDifferenceBigger <-
  simulateOptionalStoppingScenarioTwoProportions(
    safeDesign = balancedSafeDesign,
    M = 1e3,
    thetaA = 0.2,
    thetaB = 0.7
  )

## -----------------------------------------------------------------------------
plotHistogramDistributionStoppingTimes(safeSim = optionalStoppingResultTrueDifferenceBigger, 
                                       nPlan = balancedSafeDesign[["nPlan"]]["nBlocksPlan"],
                                       deltaTrue = 0.5)

## -----------------------------------------------------------------------------
optionalStoppingResultTrueDifferenceBigger[["powerOptioStop"]]

## -----------------------------------------------------------------------------
#set a seed for the simulations
set.seed(1082021)
optionalStoppingResultTrueDifferenceNull <- simulateOptionalStoppingScenarioTwoProportions(safeDesign = balancedSafeDesign, 
                                                    M = 1e3, 
                                                    thetaA = 0.5, 
                                                    thetaB = 0.5)

## -----------------------------------------------------------------------------
plotHistogramDistributionStoppingTimes(safeSim = optionalStoppingResultTrueDifferenceNull, 
                                       nPlan = balancedSafeDesign[["nPlan"]]["nBlocksPlan"],
                                       deltaTrue = 0)

## -----------------------------------------------------------------------------
optionalStoppingResultTrueDifferenceNull[["powerOptioStop"]]

## ---- echo = FALSE------------------------------------------------------------
# optionalStoppingWrongFisher <- simulateIncorrectStoppingTimesFisher(thetaA = 0.5,
#                                      thetaB = 0.5,
#                                      alpha = 0.05,
#                                      na = balancedSafeDesign[["nPlan"]][["na"]],
#                                      nb = balancedSafeDesign[["nPlan"]][["nb"]],
#                                      maxSimStoptime = balancedSafeDesign[["nPlan"]][["nBlocksPlan"]],
#                                      M = 1e3,
#                                      numberForSeed = 1082021)
# save(optionalStoppingWrongFisher, file = "safe2x2VignetteData/safe2x2SimFisher.Rdata")
load("safe2x2VignetteData/safe2x2SimFisher.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  optionalStoppingWrongFisher <-
#    simulateIncorrectStoppingTimesFisher(
#      thetaA = 0.5,
#      thetaB = 0.5,
#      alpha = 0.05,
#      na = balancedSafeDesign[["nPlan"]][["na"]],
#      nb = balancedSafeDesign[["nPlan"]][["nb"]],
#      maxSimStoptime = balancedSafeDesign[["nPlan"]][["nBlocksPlan"]],
#      M = 1e3,
#      numberForSeed = 1082021
#    )

## -----------------------------------------------------------------------------
mean(optionalStoppingWrongFisher[["rejections"]] == 1)

## -----------------------------------------------------------------------------
eValuesNotRejected <-
  optionalStoppingResult[["eValues"]][!optionalStoppingResult[["allSafeDecisions"]]]
eValuesNotRejectedNull <-
  optionalStoppingResultTrueDifferenceNull[["eValues"]][!optionalStoppingResultTrueDifferenceNull[["allSafeDecisions"]]]

## ---- echo = FALSE------------------------------------------------------------
trueHist <- graphics::hist(x = eValuesNotRejected, plot = FALSE)
nullHist <- graphics::hist(x = eValuesNotRejectedNull, plot = FALSE)
oldPar <- graphics::par(cex.main=1.5, mar=c(5, 6, 4, 4)+0.1, mgp=c(3.5, 1, 0), cex.lab=1.5,
              font.lab=2, cex.axis=1.3, bty="n", las=1)
graphics::plot(nullHist, xlim = c(0, max(eValuesNotRejected, eValuesNotRejectedNull)),
               freq = FALSE, col = "blue", density = 20, angle = 45, xlab = "e-values",
               main = "Histogram of e-values where null not rejected")
graphics::plot(trueHist, add = TRUE, freq = FALSE, col = "red", density = 20,
               angle = -45)
graphics::legend(x = "topright", legend = c("True delta: null", "True delta: design"), fill = c("blue", "red"))
par(oldPar)

## -----------------------------------------------------------------------------
promisingEValues <- eValuesNotRejected[eValuesNotRejected >= 10]
followUpDesign <- designSafeTwoProportions(na = 1, nb = 1, nBlocksPlan = 30)
followUpResults <- simulateOptionalStoppingScenarioTwoProportions(followUpDesign, 
                                                                  M = length(promisingEValues), 
                                                                  thetaA = 0.2, thetaB = 0.5)
newEValues <- followUpResults[["eValues"]] * promisingEValues

## ---- echo = FALSE, cache = FALSE---------------------------------------------
trueHist <- graphics::hist(x = newEValues, plot = FALSE)
oldPar <- graphics::par(cex.main=1.5, mar=c(5, 6, 4, 4)+0.1, mgp=c(3.5, 1, 0), cex.lab=1.5,
                        font.lab=2, cex.axis=1.3, bty="n", las=1)
graphics::plot(trueHist, xlim = c(0, max(newEValues)),
               freq = FALSE, col = "blue", density = 20, angle = 45, xlab = "e-values",
               main = "Histogram of e-values after continuing data collection")
par(oldPar)

## ---- cache = FALSE-----------------------------------------------------------
sum(newEValues >= 20)/length(promisingEValues)

## -----------------------------------------------------------------------------
promisingEValuesNull <-
  eValuesNotRejectedNull[eValuesNotRejectedNull >= 1]
followUpDesign <-
  designSafeTwoProportions(na = 1,
                           nb = 1,
                           nBlocksPlan = 30)
followUpResults <-
  simulateOptionalStoppingScenarioTwoProportions(
    followUpDesign,
    M = length(promisingEValuesNull),
    thetaA = 0.5,
    thetaB = 0.5
  )
newEValuesNull <-
  followUpResults[["eValues"]] * promisingEValuesNull

## ---- echo = FALSE, cache = FALSE---------------------------------------------
nullHist <- graphics::hist(x = newEValuesNull, plot = FALSE)
oldPar <- graphics::par(cex.main=1.5, mar=c(5, 6, 4, 4)+0.1, mgp=c(3.5, 1, 0), cex.lab=1.5,
                        font.lab=2, cex.axis=1.3, bty="n", las=1)
graphics::plot(nullHist, xlim = c(0, max(newEValuesNull)),
               freq = FALSE, col = "blue", density = 20, angle = 45, xlab = "e-values",
               main = "Histogram of e-values after continuing data collection under the null")
par(oldPar)

## ---- eval = FALSE------------------------------------------------------------
#  plotConfidenceSequenceTwoProportions(ya = ya,
#                                       yb = yb,
#                                       safeDesign = balancedSafeDesign,
#                                       differenceMeasure = "difference",
#                                       precision = 100,
#                                       trueDifference = 0.3)

## ---- echo = FALSE------------------------------------------------------------
# coverageSimResult <- simulateCoverageDifferenceTwoProportions(successProbabilityA = 0.2,
#                                            trueDelta = 0.3,
#                                            safeDesign = balancedSafeDesign,
#                                            numberForSeed = 1082021)
# save(coverageSimResult, file = "safe2x2VignetteData/safe2x2Coverage.Rdata")
load("safe2x2VignetteData/safe2x2Coverage.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  coverageSimResult <- simulateCoverageDifferenceTwoProportions(successProbabilityA = 0.2,
#                                             trueDelta = 0.3,
#                                             safeDesign = balancedSafeDesign,
#                                             numberForSeed = 1082021)

## -----------------------------------------------------------------------------
print(coverageSimResult)

## -----------------------------------------------------------------------------
log(0.5/0.5 * 0.8/0.2)

## ---- echo = FALSE------------------------------------------------------------
# confidenceBoundOdds <- computeConfidenceBoundForLogOddsTwoProportions(ya = ya,
#                                            yb = yb,
#                                            safeDesign = balancedSafeDesign,
#                                            bound = "lower",
#                                            precision = 100,
#                                            deltaStart = 0.001,
#                                            deltaStop = 2)
# save(confidenceBoundOdds, file = "safe2x2VignetteData/safe2x2ConfidenceOdds.Rdata")
load("safe2x2VignetteData/safe2x2ConfidenceOdds.Rdata")

## ---- eval = FALSE------------------------------------------------------------
#  confidenceBoundOdds <- computeConfidenceBoundForLogOddsTwoProportions(ya = ya,
#                                             yb = yb,
#                                             safeDesign = balancedSafeDesign,
#                                             bound = "lower",
#                                             precision = 100,
#                                             deltaStart = 0.001,
#                                             deltaStop = 2)

## -----------------------------------------------------------------------------
print(confidenceBoundOdds)

## -----------------------------------------------------------------------------
computeConfidenceBoundForLogOddsTwoProportions(ya = ya,
                                           yb = yb, 
                                           safeDesign = balancedSafeDesign,
                                           bound = "upper", 
                                           precision = 10, 
                                           deltaStart = -0.01, 
                                           deltaStop = -2)

## ---- eval = FALSE------------------------------------------------------------
#  plotConfidenceSequenceTwoProportions(ya = ya,
#                                       yb = yb,
#                                       safeDesign = balancedSafeDesign,
#                                       differenceMeasure = "odds",
#                                       precision = 100,
#                                       trueDifference = log(0.5/0.5 * 0.8/0.2),
#                                       deltaStart = 0.001,
#                                       deltaStop = 2)

