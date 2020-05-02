#' Multiple imputation for categorical variables based on predictions
#' of random forest
#'
#' @description
#' Part of project \code{RfEmpImp}, the function \code{mice.impute.rfpred.cate}
#' is for mixed categorical variables, performing imputation based on predicted
#' probabilities.
#'
#' @details
#' \code{RfEmpImp} Imputation sampler for: continuous variables based on the
#' empirical distribution of out-of-bag prediction errors of random forests;
#' categorical variables based on probability machines
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
#' @param num.trees Number of trees to build, default to \code{10}; overrides
#' \code{num.trees.cont} and \code{num.trees.cate}
#'
#' @param num.trees.cont Number of trees to build for continuous variables,
#' default to \code{10}
#'
#' @param sym.cont Logical, \code{TRUE} for assuming symmetric empirical error
#' distribution, \code{FALSE} for asymmetric empirical error distribution,
#' default to \code{TRUE}. This option is ignored when \code{emp.err.cont} is
#' set to \code{FALSE}.
#'
#' @param num.trees.cate Number of trees to build for categorical variables,
#' default to \code{10}
#'
#' @param use.pred.prob.cate Logical, \code{TRUE} for assigning categories based
#' on predicted probabilities, \code{FALSE} for imputation based on majority
#' votes, default to \code{TRUE}
#'
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014)
#'
#' @param emp.err.cont Logical, \code{TRUE} for using
#' empirical error; \code{FALSE}, for assuming normal distribution for the
#' prediction error, the variance equals to overall out-of-bag prediction error,
#' i.e. mean squared error (see Shah et al. 2014).
#'
#' @param alpha.emp The "significance level" for empirical error
#' distribution in prevention for outliers.
#' Set alpha = 0.05 to use 95\% confidence level for empirical error
#' distribution. Default is \code{0.0}, and the empirical error distribution is
#' kept intact.
#'
#' @param ... Other arguments to pass down
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#'
#' @name mice.impute.rfemp
#' @order 1
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#' Zhang, Haozhe, et al. "Random Forest Prediction Intervals."
#' The American Statistician just-accepted (2019): 1-20.
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
