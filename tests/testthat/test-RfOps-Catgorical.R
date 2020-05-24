context("RfOps - node in single tree for categorical variable")

# Set-up
set.seed(2020)
NUM_OBS <- 200
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(
    testData,
    y =  {
        yTemp <- 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS)
        y <-
            cut(
                yTemp,
                breaks = quantile(
                    yTemp,
                    probs = seq(0, 1, by = 0.25),
                    na.rm = TRUE,
                    names = FALSE
                ),
                include.lowest = TRUE,
                right = TRUE,
                labels = c("A", "B", "C", "D")
            )
        y
    })
num.trees <- 1

test_that("prediction for a classification tree is the most frequent class", {
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
    # Test that the predictions for a classification tree is the class with
    # the most frequency
    reCalc <- rep(x = obsSelected, times = inbagFreq)
    maxLv <- names(table(reCalc))[1]
    trueRed <- predObjPred$predictions[1]
    expect_true(as.character(trueRed) == maxLv)
})


test_that("predicted probabilites works", {
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        probability = TRUE,
        oob.error = FALSE,
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
    # Test that the predicted probabilities for a classification tree
    reCalc <- rep(x = obsSelected, times = inbagFreq)
    lvs <- table(factor(reCalc, levels = levels(testData$y)))
    lvsProb <- lvs / sum(lvs)
    trueRed <- predObjPred$predictions[1, , drop = TRUE]
    expect_true(all(abs((trueRed - lvsProb) < 1e-5)))
})


context("RfOps - node in forest for categorical variable")

num.trees <- 2

test_that("predicted probabilites works", {
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        probability = TRUE,
        oob.error = FALSE,
        num.threads = 1
    )
    # Get predictions
    predObjPred <- predict(rfObj, data = testData)
    predObjPredAll <- predict(rfObj, data = testData, predict.all = TRUE)
    predObjPredVal <- predObjPred$predictions
    predObjPredAllVal <- rowMeans(predObjPredAll$predictions, dims = 2)
    expect_true(all(abs((predObjPredVal - predObjPredAllVal) < 1e-5)))
})
