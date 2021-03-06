#' Univariate sampler function for categorical variables for prediction-based
#' imputation, using predicted probabilities of random forest
#'
#' @description
#' Please note that functions with names starting with "mice.impute" are
#' exported to be visible for the mice sampler functions. Please do not call
#' these functions directly unless you know exactly what you are doing.
#'
#' For categorical variables only.
#'
#' Part of project \code{RfEmpImp}, the function \code{mice.impute.rfpred.cate}
#' is for categorical variables, performing imputation based on predicted
#' probabilities for the categories.
#'
#' @details
#' \code{RfEmpImp} Imputation sampler for: categorical variables based on
#' predicted probabilities.
#'
#' @param y Vector to be imputed.
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
#' default to \code{10}.
#'
#' @param use.pred.prob.cate Logical, \code{TRUE} for assigning categories based
#' on predicted probabilities, \code{FALSE} for imputation based on random draws
#' from predictions of classification trees, default to \code{TRUE}. Note that
#' if \code{forest.vote.cate = TRUE}, then this option is invalid.
#'
#' @param forest.vote.cate Logical, \code{TRUE} for assigning categories based
#' on majority votes of random forests, \code{FALSE} for imputation based on
#' control of option \code{use.pred.prob.cate}, default to \code{FALSE}.
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
#' @param ... Other arguments to pass down.
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}.
#'
#' @name mice.impute.rfpred.cate
#'
#' @author Shangzhi Hong
#'
#' @references
#' Hong, Shangzhi, et al. "Multiple imputation using chained random forests."
#' Preprint, submitted April 30, 2020. https://arxiv.org/abs/2004.14823.
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' Malley, James D., et al. "Probability machines." Methods of information
#' in medicine 51.01 (2012): 74-81.
#'
#' @examples
#' # Prepare data
#' mtcars.catmcar <- mtcars
#' mtcars.catmcar[, c("gear", "carb")] <-
#'     gen.mcar(mtcars.catmcar[, c("gear", "carb")], warn.empty.row = FALSE)
#' mtcars.catmcar <- conv.factor(mtcars.catmcar, c("gear", "carb"))
#' # Perform imputation
#' impObj <- mice(mtcars.catmcar, method = "rfpred.cate", m = 5, maxit = 5,
#' maxcor = 1.0, eps = 0,
#' remove.collinear = FALSE, remove.constant = FALSE,
#' printFlag = FALSE)
#'
#' @export
mice.impute.rfpred.cate <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cate = 10,
    use.pred.prob.cate = TRUE,
    forest.vote.cate = FALSE,
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
    if (isTRUE(forest.vote.cate)) {
        # Construct predictions based on major votes, less variant
        # TODO: Let ranger handle unused arguments after v0.12.3
        # rangerObj <- suppressWarnings(ranger(
        #     x = xObs,
        #     y = yObs,
        #     oob.error = FALSE,
        #     num.trees = num.trees.cate,
        #     num.threads = num.threads,
        #     ...))
        rangerObj <- rangerCallerSafe(
            x = xObs,
            y = yObs,
            oob.error = FALSE,
            num.trees = num.trees.cate,
            num.threads = num.threads,
            ...)
        impVal <- predictions(predict(rangerObj, xMis))
    } else if (isTRUE(use.pred.prob.cate)) {
        # Construct predictions based on probabilities
        # Suppress warnings like:
        # "Dropped unused factor level(s) in dependent variable"
        # TODO: Let ranger handle unused arguments after v0.12.3
        # rangerObjProb <- suppressWarnings(ranger(
        #     x = xObs,
        #     y = yObs,
        #     probability = TRUE,
        #     oob.error = FALSE,
        #     num.trees = num.trees.cate,
        #     num.threads = num.threads,
        #     ...))
        rangerObjProb <- rangerCallerSafe(
            x = xObs,
            y = yObs,
            probability = TRUE,
            oob.error = FALSE,
            num.trees = num.trees.cate,
            num.threads = num.threads,
            ...)
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
        # Imputation using random draws from predictions of trees
        # TODO: Let ranger handle unused arguments after v0.12.3
        # rangerObj <- suppressWarnings(ranger(
        #     x = xObs,
        #     y = yObs,
        #     oob.error = FALSE,
        #     num.trees = num.trees.cate,
        #     num.threads = num.threads,
        #     ...))
        rangerObj <- rangerCallerSafe(
            x = xObs,
            y = yObs,
            oob.error = FALSE,
            num.trees = num.trees.cate,
            num.threads = num.threads,
            ...)
        misPredMat <- predictions(predict(rangerObj, xMis, predict.all = TRUE))
        impValChar <- apply(
            X = misPredMat,
            MARGIN = 1,
            FUN = function(x) sample(x = x, size = 1))
        impVal <- factor(x = levels(y)[impValChar], levels = levels(y))
    }
    if (yIsLogical) impVal <- as.logical(impVal == "TRUE")
    return(impVal)
}
