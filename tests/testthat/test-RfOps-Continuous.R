context("RfOps - node in single tree for continuous variable")

# Set-up
set.seed(2020)
NUM_OBS <- 100
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)
testData <- transform(testData,
                      y = 4 + 6 * x1 + 8 * x2 + 10 * x3 + rnorm(NUM_OBS))

test_that("single tree node prediction works for continuous variable", {
    num.trees <- 1
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        keep.inbag = TRUE,
        num.threads = 1
    )
    # Get predictions
    predObjPred <- predict(rfObj, data = testData)
    # Get Node IDs
    predObjNode <-
        predict(rfObj, data = testData, type = "terminalNodes")
    predNodeIds <- predObjNode$predictions[, 1, drop = TRUE]
    obs1NodeId <- predNodeIds[1]
    sameNodeObsIdx <- which(predNodeIds == obs1NodeId)
    inbagFreqAll <- rfObj$inbag.counts[[1]]
    inbagFreq <- inbagFreqAll[sameNodeObsIdx]
    obsSelected <- testData[["y"]][sameNodeObsIdx]
    # Test that the predictions for a regression tree is the average of
    # observations under the prediction node for continuous variable
    reCalc <- sum(obsSelected * inbagFreq) / sum(inbagFreq)
    trueRed <- predObjPred$predictions[1]
    expect_true(abs(reCalc - trueRed) < 1e-5)
})


context("RfOps - nodes in forest for continuous variable")

test_that("forest prediction works for continuous variable", {
    num.trees <- 2
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        num.threads = 1
    )
    # Get predictions
    predObjPred <- predict(rfObj, data = testData)
    predObjPredAll <- predict(rfObj, data = testData, predict.all = TRUE)
    predObjPredVal <- predObjPred$predictions
    predObjPredAllVal <- rowMeans(predObjPredAll$predictions)
    expect_true(all(abs(predObjPredVal - predObjPredAllVal) < 1e-5))
})

