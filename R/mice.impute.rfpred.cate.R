#' Multiple imputation for categorical variables based on predictions
#' of random forest
#'
#' @description
#' Part of project \code{RfEmpImp}, the function \code{mice.impute.rfpred.cate}
#' is for mixed categorical variables, performing imputation based on predicted
#' probabilities.
#'
#' @details
#' \code{RfEmpImp} Imputation sampler for: continuous variables based on
#' probability machines
#'
#' @param y Vector to be imputed
#'
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#'
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#'
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#'
#' @param num.trees.cate Number of trees to build for categorical variables,
#' default to \code{10}
#'
#' @param use.pred.prob.cate Logical, \code{TRUE} for assigning categories based
#' on predicted probabilities, \code{FALSE} for imputation based on majority
#' votes, default to \code{TRUE}
#'
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' multiple imputation, i.e. accommodating sampling variation in estimating
#' population regression parameters (see Shah et al. 2014).
#' It should be noted that if \code{TRUE}, this option is in effect even if the
#' number of trees is set to one.
#'
#' @param num.threads Number of threads for parallel computing. The default is
#' \code{num.threads = NULL} and all the processors available can be used.
#'
#' @param ... Other arguments to pass down
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#'
#' @name mice.impute.rfemp
#' @order 1
#'
#' @author Shangzhi Hong
#'
#' @references
#' Hong, Shangzhi, et al. "Multiple imputation using chained random forests"
#' arXiv:2004.14823.
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' Malley, James D., et al. "Probability machines." Methods of information
#' in medicine 51.01 (2012): 74-81.
#'
#' @export
mice.impute.rfpred.cate <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cate = 10,
    use.pred.prob.cate = TRUE,
    pre.boot = TRUE,
    num.threads = NULL,
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
        # TODO: Add `...` back to ranger when updates available
        rangerObjProb <- suppressWarnings(ranger(
            x = xObs,
            y = yObs,
            probability = TRUE,
            oob.error = FALSE,
            num.trees = num.trees.cate,
            num.threads = num.threads))
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
