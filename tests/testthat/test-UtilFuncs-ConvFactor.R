# Single tree -------------------------------------------------------------

context("UtilFuncs - conv.factor for data frame")

# Set-up
set.seed(2020)
NUM_OBS <- 100
testData <- data.frame(
    x1 = sample(
        x = c("A", "B", "C"),
        size = NUM_OBS,
        replace = TRUE),
    x2 = sample(
        x = c("A", "B", "C", "D"),
        size = NUM_OBS,
        replace = TRUE),
    x3 = sample(
        x = c("A", "B", "C", "D", "E"),
        size = NUM_OBS,
        replace = TRUE)
)

test_that("conv.factor works", {
    conv1 <- conv.factor(data = testData)
    factorLoc1 <- sapply(X = conv1, FUN = is.factor)
    expect_true(is.data.frame(conv1))
    expect_true(all(factorLoc1 == c(TRUE, TRUE, TRUE)))
    conv2 <- conv.factor(data = testData, convNames = "x1")
    factorLoc2 <- sapply(X = conv2, FUN = is.factor)
    expect_true(is.data.frame(conv2))
    expect_true(all(factorLoc2 == c(TRUE, FALSE, FALSE)))
    conv3 <- conv.factor(data = testData, exceptNames = "x2")
    factorLoc3 <- sapply(X = conv3, FUN = is.factor)
    expect_true(is.data.frame(conv3))
    expect_true(all(factorLoc3 == c(TRUE, FALSE, TRUE)))
})


