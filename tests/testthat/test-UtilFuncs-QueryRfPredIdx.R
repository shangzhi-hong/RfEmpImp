# Single tree -------------------------------------------------------------

context("UtilFuncs - query.rf.pred.idx for single tree and continuous variables")

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

num.trees <- 1
# Build needed objects
rfObj <- ranger(
    y ~ x1 + x2 + x3,
    data = testData,
    num.trees = num.trees,
    keep.inbag = TRUE,
    num.threads = 1
)
outIdxListTotal <- query.rf.pred.idx(obj = rfObj, data = testData)

test_that("output contents are correct for a single tree", {
    expect_equal(length(outIdxListTotal), num.trees)
})

test_that("out-of-bag freqs can be recalculated for a single tree", {
    treeIdx <- 1
    outIdxList <- outIdxListTotal[[treeIdx]]
    oobTestInd <- logical(length(outIdxList))
    for (i in seq_len(length(outIdxList))) {
        oobTestInd[i] <- (!(i %in% outIdxList[[i]]))
    }
    inbagFreqMat <- matrix(
        as.integer(unlist(rfObj[["inbag.counts"]][[treeIdx]])),
        ncol = 1,
        byrow = FALSE)
    oobIndVec <- rowSums(inbagFreqMat) == 0L
    expect_true(all(oobTestInd == oobIndVec))
})

test_that("predicted values can be recalculated for a single tree", {
    treeIdx <- 1
    outIdxList <- outIdxListTotal[[treeIdx]]
    recalcPred <- sapply(
        X = outIdxList,
        FUN = function(x) {
            yVec <- testData[["y"]][x]
            return(mean(yVec))
        }
    )
    predRf <- predict(rfObj, data = testData[, !colnames(testData) %in% "y"])
    expect_true(all(abs(predRf$predictions - recalcPred) < 1e-5))
})


# Multiple trees ----------------------------------------------------------

context("UtilFuncs - query.rf.pred.idx for multiple trees and continuous variables")

num.trees <- 5
# Build needed objects
rfObj <- ranger(
    y ~ x1 + x2 + x3,
    data = testData,
    num.trees = num.trees,
    keep.inbag = TRUE,
    num.threads = 1
)
outIdxListTotal <- query.rf.pred.idx(obj = rfObj, data = testData)

test_that("output contents are correct for multiple trees", {
    expect_equal(length(outIdxListTotal), num.trees)
})

test_that("out-of-bag freqs can be recalculated for multiple trees", {
    outIdxList <- Reduce(
        f = function(list1, list2) {
            mapply(
                FUN = function(vec1, vec2) {
                    c(vec1, vec2)
                },
                vec1 = list1,
                vec2 = list2,
                SIMPLIFY = FALSE
            )
        },
        x = outIdxListTotal
    )
    oobTestInd <- logical(length(outIdxList))
    for (i in seq_len(length(outIdxList))) {
        oobTestInd[i] <- (!(i %in% outIdxList[[i]]))
    }
    inbagFreqMat <- matrix(
        as.integer(unlist(rfObj[["inbag.counts"]])),
        ncol = num.trees,
        byrow = FALSE)
    oobIndVec <- rowSums(inbagFreqMat) == 0L
    expect_true(all(oobTestInd == oobIndVec))
})

test_that("predicted values can be recalculated for multiple trees", {
    calcList <-
        lapply(X = outIdxListTotal, FUN = function(outIdxList) {
            sapply(
                X = outIdxList,
                FUN = function(x) {
                    yVec <- testData[["y"]][x]
                    return(mean(yVec))
                }
            )
        })
    recalcPred <- rowMeans(as.data.frame(calcList))
    predRf <- predict(rfObj, data = testData[, !colnames(testData) %in% "y"])
    expect_true(all(abs(predRf$predictions - recalcPred) < 1e-5))
})


# Options -----------------------------------------------------------------


context("UtilFuncs - query.rf.pred.idx options")

test_that("options are valid for query.rf.pred.idx", {
    outIdxListNameId <- query.rf.pred.idx(
        obj = rfObj,
        data = testData,
        id.name = TRUE)
    invisible(lapply(
        X = outIdxListNameId,
        FUN = function(list) {
            expect_true(!is.null(names(list)))
        }
    ))
    outIdxListUnique <- query.rf.pred.idx(
        obj = rfObj,
        data = testData,
        unique.by.id = TRUE)
    invisible(lapply(
        X = outIdxListUnique,
        FUN = function(list) {
            expect_true(!is.null(names(list)))
            expect_true(length(names(list)) == length(unique(names(list))))
        }
    ))
})
