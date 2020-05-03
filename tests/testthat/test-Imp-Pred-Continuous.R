context("RfEmpImp for continuous variables")

# Set-up
NUM_OBS <- 50
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1))

testData <- transform(
    testData,
    y = 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS))

y <- testData[["y"]]
x <- subset(testData, select = -y)
ry <- sample(
    x = c(TRUE, FALSE),
    size = NUM_OBS,
    replace = TRUE,
    prob = c(0.5, 0.5))
wy <- !ry
num.trees.cont <- 10

test_that("rfpred.emp works for continuous variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        for (sym.dist in c(TRUE, FALSE)) {
            for (emp.err.cont in c(TRUE, FALSE)) {
                for (alpha.emp in c(-0.1, 0.0, 0.05, 0.1)) {
                    numImpOut1 <- mice.impute.rfpred.emp(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        num.trees.cont = num.trees.cont,
                        sym.dist = sym.dist,
                        emp.err.cont = emp.err.cont,
                        num.threads = 1)

                    numImpOut2 <- mice.impute.rfpred.emp(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        num.trees.cont = num.trees.cont,
                        sym.dist = sym.dist,
                        emp.err.cont = emp.err.cont,
                        num.threads = 1)
                    expect_true(all(c(is.numeric(numImpOut1),
                                      is.numeric(numImpOut2))))
                    expect_true(length(numImpOut1) == sum(wy))
                    expect_true(length(numImpOut2) == sum(wy))
                    expect_true(!anyNA(numImpOut1) && !anyNA(numImpOut2))
                    expect_true(!all(numImpOut1 == numImpOut2))
                }
            }
        }
    }
})

test_that("rfemp caller works for continuous variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        for (sym.dist in c(TRUE, FALSE)) {
            for (emp.err.cont in c(TRUE, FALSE)) {
                    for (alpha.emp in c(-0.1, 0.0, 0.05, 0.1)) {
                    numImpOut1 <- mice.impute.rfemp(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        num.trees.cont = num.trees.cont,
                        sym.dist = sym.dist,
                        emp.err.cont = emp.err.cont,
                        num.threads = 1)
                    numImpOut2 <- mice.impute.rfemp(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        num.trees.cont = num.trees.cont,
                        sym.dist = sym.dist,
                        emp.err.cont = emp.err.cont,
                        num.threads = 1)

                    expect_true(all(c(is.numeric(numImpOut1),
                                      is.numeric(numImpOut2))))
                    expect_true(length(numImpOut1) == sum(wy))
                    expect_true(length(numImpOut2) == sum(wy))
                    expect_true(!anyNA(numImpOut1) && !anyNA(numImpOut2))
                    expect_true(!all(numImpOut1 == numImpOut2))
                }
            }
        }
    }
})
