# Single tree -------------------------------------------------------------

context("UtilFuncs - query.rf.pred.val for single tree and continuous variables")

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
outValListTotal <- query.rf.pred.val(obj = rfObj, data = testData)

test_that("output contents are correct for a single tree", {
    expect_equal(length(outValListTotal), num.trees)
})

test_that("predicted values can be recalculated for a single tree", {
    treeIdx <- 1
    outValList <- outValListTotal[[treeIdx]]
    recalcPred <- sapply(X = outValList, FUN = mean)
    predRf <- predict(rfObj, data = testData[, !colnames(testData) %in% "y"])
    expect_true(all(abs(predRf$predictions - recalcPred) < 1e-5))
})


# Multiple trees ----------------------------------------------------------

context("UtilFuncs - query.rf.pred.val for multiple trees and continuous variables")

num.trees <- 5
# Build needed objects
rfObj <- ranger(
    y ~ x1 + x2 + x3,
    data = testData,
    num.trees = num.trees,
    keep.inbag = TRUE,
    num.threads = 1
)
outValListTotal <- query.rf.pred.val(obj = rfObj, data = testData)

test_that("output contents are correct for multiple trees", {
    expect_equal(length(outValListTotal), num.trees)
})

test_that("predicted values can be recalculated for multiple trees", {
    calcList <-
        lapply(X = outValListTotal, FUN = function(outValList) {
            sapply(X = outValList, FUN = mean)
        })
    recalcPred <- rowMeans(as.data.frame(calcList))
    predRf <- predict(rfObj, data = testData[, !colnames(testData) %in% "y"])
    expect_true(all(abs(predRf$predictions - recalcPred) < 1e-5))
})


# Options -----------------------------------------------------------------


context("UtilFuncs - query.rf.pred.val options")

test_that("options are valid for query.rf.pred.val", {
    outValListNameId <- query.rf.pred.val(
        obj = rfObj,
        data = testData,
        id.name = TRUE)
    invisible(lapply(
        X = outValListNameId,
        FUN = function(list) {
            expect_true(!is.null(names(list)))
        }
    ))
    outValListUnique <- query.rf.pred.val(
        obj = rfObj,
        data = testData,
        unique.by.id = TRUE)
    invisible(lapply(
        X = outValListUnique,
        FUN = function(list) {
            expect_true(!is.null(names(list)))
            expect_true(length(names(list)) == length(unique(names(list))))
        }
    ))
})
