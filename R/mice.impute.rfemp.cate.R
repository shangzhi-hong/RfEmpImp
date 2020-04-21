#' @rdname mice.impute.rfemp
#' @order 3
#'
#' @export
mice.impute.rfemp.cate <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cate = 10,
    use.pred.prob.cate = TRUE,
    pre.boot = TRUE,
    ...) {
    if (is.null(wy)) wy <- !ry
    yMisNum <- sum(wy)
    if (is.logical(y)) {
        yIsLogical <- TRUE
        y <- as.factor(y)
    } else {
        yIsLogical <- FALSE
    }
    if (isTRUE(pre.boot)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    # Short-cut
    yobsLvDrop <- droplevels(yObs)
    yObsLvNum <- nlevels(yobsLvDrop)
    if (yObsLvNum == 1) return(rep_len(yObs, length.out = yMisNum))
    xMis <- x[wy, , drop = FALSE]
    if (isTRUE(use.pred.prob.cate)) {
        # Construct predictions based on vote probs averaged over nodes.
        # Suppress warnings like:
        # "Dropped unused factor level(s) in dependent variable"
        rangerObjProb <- suppressWarnings(ranger(
            x = xObs,
            y = yObs,
            probability = TRUE,
            oob.error = FALSE,
            num.trees = num.trees.cate))
        misPredMat <- predictions(predict(rangerObjProb, xMis))
        yLevels <- colnames(misPredMat)
        impValChar <- apply(
            X = misPredMat,
            MARGIN = 1,
            FUN = function(voteProb) {
                sample(x = yLevels, size = 1, prob = voteProb)
            })
        impVal <- factor(x = impValChar, levels = levels(y))
    } else {
        # Construct predictions based on major votes, less variant
        rangerObj <- ranger(
            x = xObs,
            y = yObs,
            oob.error = FALSE,
            num.trees = num.trees.cate)
        impVal <- predictions(predict(rangerObj, xMis))
    }
    if (yIsLogical) impVal <- as.logical(impVal == "TRUE")
    return(impVal)
}
