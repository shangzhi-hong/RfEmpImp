context("mice.impute.rfnode for continuous variables")

# Set-up
set.seed(2020)
NUM_OBS <- 100
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(testData,
                      y = 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS))

y <- testData[["y"]]
x <- subset(testData, select = -y)
ry <- sample(
    x = c(TRUE, FALSE),
    size = NUM_OBS,
    replace = TRUE,
    prob = c(0.8, 0.2)
)
num.trees.node <- 2
wy <- !ry

test_that("mice.impute.rfnode works for continuous variables", {
    for (pre.boot in c(TRUE, FALSE)) {
        for (use.node.cond.dist in c(TRUE, FALSE)) {
            for (obs.eq.prob in c(TRUE, FALSE)) {
                for (num.trees.node in c(1, 5)) {
                    numImpOut1 <- mice.impute.rfnode(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        pre.boot = pre.boot,
                        use.node.cond.dist = use.node.cond.dist,
                        num.threads = 1
                    )

                    numImpOut2 <- mice.impute.rfnode(
                        y = y,
                        ry = ry,
                        x = x,
                        wy = wy,
                        pre.boot = pre.boot,
                        use.node.cond.dist = use.node.cond.dist,
                        num.threads = 1
                    )
                    expect_true(all(c(
                        is.numeric(numImpOut1),
                        is.numeric(numImpOut2)
                    )))
                    expect_true(length(numImpOut1) == sum(wy))
                    expect_true(length(numImpOut2) == sum(wy))
                    expect_true(!anyNA(numImpOut1) && !anyNA(numImpOut2))
                    expect_true(!all(numImpOut1 == numImpOut2))
                }
            }
        }
    }
})
