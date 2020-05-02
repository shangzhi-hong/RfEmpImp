context("RfEmpImp for categorical variables")

# Set-up
NUM_OBS <- 100
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(
    testData,
    y = {
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
num.trees.cate <- 10

test_that("rfpred.cate works for categorical variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        for (use.pred.prob.cate in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfpred.cate(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cate = num.trees.cate,
                pre.boot = pre.boot,
                use.pred.prob.cate = use.pred.prob.cate,
                num.threads = 1)
            catImpOut2 <- mice.impute.rfpred.cate(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cate = num.trees.cate,
                pre.boot = pre.boot,
                use.pred.prob.cate = use.pred.prob.cate,
                num.threads = 1)
            expect_true(all(c(is.factor(catImpOut1), is.factor(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }

})

test_that("rfemp caller works for categorical variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        catImpOut1 <- mice.impute.rfemp(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.cate = num.trees.cate,
            pre.boot = pre.boot)
        catImpOut2 <- mice.impute.rfemp(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.cate = num.trees.cate,
            pre.boot = pre.boot)
        expect_true(all(c(is.factor(catImpOut1), is.factor(catImpOut2))))
        expect_true(length(catImpOut1) == sum(wy))
        expect_true(length(catImpOut2) == sum(wy))
        expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
        expect_true(!all(catImpOut1 == catImpOut2))
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

test_that("rfpred.cate works for logical variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        for (use.pred.prob.cate in c(TRUE, FALSE)) {
            catImpOut1 <- mice.impute.rfpred.cate(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cate = num.trees.cate,
                pre.boot = pre.boot,
                use.pred.prob.cate = use.pred.prob.cate)
            catImpOut2 <- mice.impute.rfpred.cate(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cate = num.trees.cate,
                pre.boot = pre.boot,
                use.pred.prob.cate = use.pred.prob.cate)
            expect_true(all(c(is.logical(catImpOut1), is.logical(catImpOut2))))
            expect_true(length(catImpOut1) == sum(wy))
            expect_true(length(catImpOut2) == sum(wy))
            expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
            expect_true(!all(catImpOut1 == catImpOut2))
        }
    }

})

test_that("rfemp caller works for logical variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        catImpOut1 <- mice.impute.rfemp(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.cate = num.trees.cate,
            pre.boot = pre.boot)
        catImpOut2 <- mice.impute.rfemp(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.cate = num.trees.cate,
            pre.boot = pre.boot)
        expect_true(all(c(is.logical(catImpOut1), is.logical(catImpOut2))))
        expect_true(length(catImpOut1) == sum(wy))
        expect_true(length(catImpOut2) == sum(wy))
        expect_true(!anyNA(catImpOut1) && !anyNA(catImpOut2))
        expect_true(!all(catImpOut1 == catImpOut2))
    }
})
