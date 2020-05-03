context("InterOp - ImpFuncs")

# Set-up
NUM_OBS <- 50
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(testData,
                      y = 4 + 6 * x1 + 8 * x2 + 10 * x3 + rnorm(NUM_OBS))

ampData <- gen.mcar(testData, prop.na = 0.1, warn.empty.row = F)

num.imp <- 2
max.iter <- 2

test_that("imp.rfemp works", {
    impObj <- imp.rfemp(
        ampData,
        num.imp = num.imp,
        max.iter = max.iter,
        num.threads = 1
    )
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3))
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 4L)
})

test_that("imp.rfnode.cond works", {
    impObj <-
        imp.rfnode.cond(
            ampData,
            num.imp = num.imp,
            max.iter = max.iter,
            num.threads = 1
        )
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3))
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 4L)
})

test_that("imp.rfnode.prox works", {
    impObj <-
        imp.rfnode.prox(
            ampData,
            num.imp = num.imp,
            max.iter = max.iter,
            num.threads = 1
        )
    regObj <- with(impObj, lm(y ~ x1 + x2 + x3))
    poolObj <- pool(regObj)
    df <- reg.ests(poolObj)
    expect_equal(nrow(df), 4L)
})
