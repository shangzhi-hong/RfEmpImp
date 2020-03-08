context("Imputation for categorical variables")

# Set-up
NUM_OBS <- 100
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
            cut(yTemp, breaks = quantile(
                    yTemp,
                    probs = seq(0, 1, by = 0.20),
                    na.rm = TRUE,
                    names = FALSE),
                include.lowest = TRUE,
                right = TRUE,
                labels = c("A", "B", "C", "D", "E")
            )
        y
    }
)

y <- testData[["y"]]
x <- subset(testData, select = -y)
ry <- sample(
    x = c(TRUE, FALSE),
    size = NUM_OBS,
    replace = TRUE,
    prob = c(0.5, 0.5))
wy <- !ry
num.trees.categorical <- 10

test_that("RfEmpImp works for categorical variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
        for (use.pred.prob.categorical in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfempimp.categorical(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical,
                num.threads = 1)
            catImpOut2 <- mice.impute.rfempimp.categorical(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical,
                num.threads = 1)
            expect_true(all(c(is.factor(catImpOut1), is.factor(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }

})

test_that("RfEmpImp caller works for categorical variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
        for (use.pred.prob.categorical in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfempimp(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            catImpOut2 <- mice.impute.rfempimp(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            expect_true(all(c(is.factor(catImpOut1), is.factor(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }
})

# Logical

testData <- transform(
    testData,
    y =  {
        yTemp <- 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS)
        yQ50 <- quantile(yTemp, probs = 0.5, na.rm = TRUE, names = FALSE)
        y <- yTemp >= yQ50
        y
    }
)

y <- testData[["y"]]
x <- subset(testData, select = -y)

test_that("RfEmpImp works for logical variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
        for (use.pred.prob.categorical in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfempimp.categorical(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            catImpOut2 <- mice.impute.rfempimp.categorical(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            expect_true(all(c(is.logical(catImpOut1), is.logical(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }

})

test_that("RfEmpImp caller works for logical variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
        for (use.pred.prob.categorical in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfempimp(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            catImpOut2 <- mice.impute.rfempimp(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.categorical = num.trees.categorical,
                pre.bootstrap = pre.bootstrap,
                use.pred.prob.categorical = use.pred.prob.categorical)
            expect_true(all(c(is.logical(catImpOut1), is.logical(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }
})
