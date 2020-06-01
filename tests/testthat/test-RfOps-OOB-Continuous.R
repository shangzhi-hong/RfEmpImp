context("RfOps - OOB prediction for continuous variable")

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

test_that("out-of-bag prediction is NaN for in-bag observations", {
    num.trees <- 1
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        keep.inbag = TRUE,
        num.threads = 1)
    predObjPred <- predict(rfObj, data = testData)
    inbagFreqMat <- matrix(as.integer(unlist(rfObj[["inbag.counts"]])),
                           ncol = num.trees,
                           byrow = FALSE)
    inbagObsInd <- rowSums(inbagFreqMat) != 0L
    expect_true(all(is.nan(rfObj$predictions) == inbagObsInd))
    expect_true(all(is.na(rfObj$predictions) == inbagObsInd))
})

test_that("out-of-bag mean squared error works", {
    num.trees <- 1
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        keep.inbag = TRUE,
        num.threads = 1)
    predVecTot <- rfObj[["predictions"]]
    trueVec <- testData$y[!is.na(predVecTot)]
    predVec <- predVecTot[!is.na(predVecTot)]
    oobMseRecalc <- mean((trueVec - predVec) ** 2)
    oobMse <- rfObj$prediction.error
    expect_true(abs(oobMseRecalc - oobMse) < 1e-5)
})

test_that("out-of-bag prediction works", {
    num.trees <- 5
    rfObj <- ranger(
        y ~ x1 + x2 + x3,
        data = testData,
        num.trees = num.trees,
        keep.inbag = TRUE,
        num.threads = 1
    )
    predObjPred <- predict(rfObj, data = testData)
    inbagFreqMat <-
        matrix(as.integer(unlist(rfObj[["inbag.counts"]])),
               ncol = num.trees,
               byrow = FALSE)
    inbagStat0 <- 0 < rowSums(inbagFreqMat == 0L)
    inbagStat1 <- rowSums(inbagFreqMat == 0L) < num.trees
    # Select observations only OOB in some trees
    useIdxVec <- which((inbagStat0 & inbagStat1))
    useIdxLimited <- sample(x = useIdxVec, size = 5)
    for (useIdx in useIdxLimited) {
        # OOB indicator for single observation
        oobIndSingleObs <- inbagFreqMat[useIdx, , drop = TRUE] == 0L
        # Get observations associated with each terminal node
        predObsIdxList <- query.rf.pred.idx(rfObj, testData)
        # Get predictions for each tree
        predictionsFromTrees <- mapply(
            FUN = function(inList, oobInd, idx, obsVal) {
                if (oobInd) {
                    valSelected <- obsVal[inList[[idx]]]
                    return(mean(valSelected))
                } else {
                    return(NA)
                }
            },
            inList = predObsIdxList,
            oobInd = oobIndSingleObs,
            MoreArgs = list(idx = useIdx,
                            obsVal = testData$y),
            SIMPLIFY = TRUE
        )
        reCalcOobPred <- mean(predictionsFromTrees, na.rm = TRUE)
        rangerOobPred <- rfObj$predictions[useIdx]
        # OOB prediction is the average of predictions made by trees that
        # the target observation is OOB of
        expect_true(abs(reCalcOobPred - rangerOobPred) < 1e-5)
    }
})
