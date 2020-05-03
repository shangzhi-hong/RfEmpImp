context("InterOp - ImpFuncs")

# Set-up
NUM_OBS <- 20
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(testData,
                      y = 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS))

testDataAmp <- gen.mcar(testData, warn.empty.row = F)

test_that("imp.rfemp works", {
    impObj <- imp.rfemp(testDataAmp, num.imp = 2, max.iter = 2,
                       num.threads = 1)
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3 + 0))
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 3L)
})

test_that("imp.rfnode.cond works", {
    impObj <- imp.rfnode.cond(testDataAmp, num.imp = 2, max.iter = 2,
                             num.threads = 1)
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3 + 0))
    poolObj <- pool(regObj)
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 3L)
})

test_that("imp.rfemp works", {
    impObj <- imp.rfnode.prox(testDataAmp, num.imp = 2, max.iter = 2,
                             num.threads = 1)
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3 + 0))
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 3L)
})
