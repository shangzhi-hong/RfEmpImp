#' Multiple imputation  based on empirical distribution for continuous
#' variables, sampler adapter for MICE
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
#' @param num.trees.continuous Number of trees to build for imputing continuous
#' variables, default to \code{10}
#'
#' @param symmetry.continuous Logical indicator, \code{TRUE} for
#' assuming symmetry error distribution, \code{FALSE}, default to \code{FALSE}
#'
#' @param pre.bootstrap Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014).
#'
#' @param empirical.error.continuous Logical indicator, \code{TRUE} for using
#' empirical error; \code{FALSE}, for assuming normal distribution for error,
#' the variance equals to overall out-of-bag prediction error,
#' i.e. mean squared error (see Shah et al. 2014).
#'
#' @param alpha.empirical The "significance level" for empirical error
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
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#' Zhang, Haozhe, et al. "Random Forest Prediction Intervals."
#' The American Statistician (2019)
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' @export
mice.impute.rfempimp.continuous <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.continuous = 10,
    symmetry.continuous = TRUE,
    pre.bootstrap = TRUE,
    empirical.error.continuous = TRUE,
    alpha.empirical = 1.0,
    ...
    ) {
    if (is.null(wy)) wy <- !ry

    yObsInd <- ry
    yMisInd <- wy
    yMisNum <- sum(yMisInd)
    if (isTRUE(pre.bootstrap)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[yObsInd][bootIdx]
        xObs <- x[yObsInd, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[yObsInd]
        xObs <- x[yObsInd, , drop = FALSE]
    }
    xMis <- x[yMisInd, , drop = FALSE]
    # Long name to avoid conflict
    # yVarName <- "___VAR_NAME_FOR_RF_IMPUTATION_BY_SZ_HONG_2020___"
    # dataRf <- cbind(xObs, yObs)
    # colnames(dataRf) <- c(colnames(xObs), yVarName)
    rangerObj <- ranger(
        x = xObs,
        y = yObs,
        oob.error = TRUE,
        num.trees = num.trees.continuous)
    misPredVal <- predictions(predict(rangerObj, xMis))
    if (empirical.error.continuous) {
        # Get OOB error
        oobErrEmp <-  yObs - rangerObj[["predictions"]]
        # To fix "NaN"s in OOB error due to small tree number
        oobErrEmp <- oobErrEmp[!is.na(oobErrEmp)]
        if (isTRUE(symmetry.continuous)) {
            oobErrEmpAbs <- abs(oobErrEmp)
            if (isTRUE(alpha.empirical > 0 && alpha.empirical < 1)) {
                oobErrorEmpHi <- quantile(oobErrEmpAbs,
                                          probs = (1 - alpha.empirical),
                                          na.rm = TRUE,
                                          names = FALSE)
                oobErrEmpAbs <- oobErrEmpAbs[oobErrEmpAbs < oobErrorEmpHi]
            }
            noiseAbs <- sample(x = oobErrEmpAbs, size = yMisNum, replace = TRUE)
            signVec <- sample(x = c(1L, -1L), size = yMisNum, replace = TRUE)
            noiseVec <- signVec * noiseAbs
        } else {
            if (isTRUE(alpha.empirical > 0 && alpha.empirical < 1)) {
                oobErrorEmpLimit <- quantile(
                    oobErrEmp,
                    probs = c((alpha.empirical / 2),
                              (1 - alpha.empirical /2)),
                    na.rm = TRUE,
                    names = FALSE)
                oobErrEmp <- oobErrEmp[
                    oobErrEmp > oobErrorEmpLimit[1] &
                    oobErrEmp < oobErrorEmpLimit[2]]
            }
            noiseVec <- sample(
                x = oobErrEmp,
                size = yMisNum,
                replace = TRUE)
        }
        impVal <- misPredVal + noiseVec
    } else {
        # Use overall out-of-bag prediction error
        impVal <- rnorm(length(misPredVal),
                        mean = misPredVal,
                        sd = sqrt(rangerObj[["prediction.error"]]))
    }
    return(impVal)
}
