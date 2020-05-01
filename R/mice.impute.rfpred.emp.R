#' Multiple imputation using chained random forests: RfPred.Emp
#'
#' @description
#' For continuous variables only.
#'
#' This function is for \code{RfPred.Emp}
#' multiple imputation method, adapter for \code{mice} samplers.
#' In the \code{mice()} function, set \code{method = "rfpred.emp"} to call it.
#'
#' The function performs multiple imputation based on the empirical distribution
#' of out-of-bag prediction errors of random forests.
#'
#' @details
#' \code{RfPred.Emp} imputation sampler.
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
#' @param num.trees Number of trees to build, default to \code{10}.
#'
#' @param num.trees.cont Number of trees to build for continuous variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param emp.err.cont Logical, \code{TRUE} for using
#' empirical error; \code{FALSE}, for assuming normal distribution for the
#' prediction error, the variance equals to overall out-of-bag prediction error,
#' i.e. mean squared error (see Shah et al. 2014).
#'
#' @param alpha.emp The "significance level" for empirical distribution of
#' prediction errors, can be used for prevention for outliers (useful for highly
#' skewed variables). For example, set alpha = 0.05 to use 95\% confidence level
#' for empirical distribution of prediction errors.
#' Default is \code{0.0}, and the empirical error distribution is kept intact.
#'
#' @param sym.cont Logical, \code{TRUE} for assuming symmetric distribution of
#' empirical prediction errors, \code{FALSE} for asymmetric distribution of
#' empirical prediction errors, default to \code{TRUE}.
#' This option is invalid when \code{emp.err.cont} is set to \code{FALSE}.
#'
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' multiple imputation, i.e. accommodating sampling variation in estimating
#' population regression parameters (see Shah et al. 2014).
#' It should be noted that if \code{TRUE}, this option is in effect even if the
#' number of trees is set to one.
#'
#' @param ... Other arguments to pass down.
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}.
#'
#' @name mice.impute.rfpred.emp
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#' Zhang, Haozhe, et al. "Random Forest Prediction Intervals."
#' The American Statistician (2019): 1-20.
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' Malley, James D., et al. "Probability machines." Methods of information
#' in medicine 51.01 (2012): 74-81.
#'
#' @examples
#' \donttest{
#' impRfPredEmp <- mice(nhanes, method = "rfpred.emp", m = 10,
#' max.iter = 10, maxcor = 1.0, printFlag = FALSE)#' }
#'
#' @export
mice.impute.rfpred.emp <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cont = 10,
    sym.cont = TRUE,
    pre.boot = TRUE,
    emp.err.cont = TRUE,
    alpha.emp = 0.0,
    ...
    ) {
    if (is.null(wy)) wy <- !ry
    yMisNum <- sum(wy)
    if (isTRUE(pre.boot)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    xMis <- x[wy, , drop = FALSE]
    rfObj <- ranger(
        x = xObs,
        y = yObs,
        oob.error = TRUE,
        num.trees = num.trees.cont)
    misPredVal <- predictions(predict(rfObj, xMis))
    if (emp.err.cont) {
        # Get empirical OOB prediction errors
        oobErrEmp <-  yObs - rfObj[["predictions"]]
        # To fix "NaN"s in OOB error due to small tree number
        oobErrEmp <- oobErrEmp[!is.na(oobErrEmp)]
        if (isTRUE(sym.cont)) {
            oobErrEmpAbs <- abs(oobErrEmp)
            if (isTRUE(alpha.emp > 0 && alpha.emp < 1)) {
                oobErrorEmpHi <- quantile(oobErrEmpAbs,
                                          probs = (1 - alpha.emp),
                                          na.rm = TRUE,
                                          names = FALSE)
                oobErrEmpAbs <- oobErrEmpAbs[oobErrEmpAbs < oobErrorEmpHi]
            }
            noiseAbs <- sample(x = oobErrEmpAbs, size = yMisNum, replace = TRUE)
            signVec <- sample(x = c(1L, -1L), size = yMisNum, replace = TRUE)
            noiseVec <- signVec * noiseAbs
        } else {
            if (isTRUE(alpha.emp > 0 && alpha.emp < 1)) {
                oobErrorEmpLimit <- quantile(
                    oobErrEmp,
                    probs = c((alpha.emp / 2), (1 - alpha.emp / 2)),
                    na.rm = TRUE,
                    names = FALSE)
                oobErrEmp <- oobErrEmp[
                    oobErrEmp > oobErrorEmpLimit[1] &
                    oobErrEmp < oobErrorEmpLimit[2]]
            }
            noiseVec <- sample(x = oobErrEmp, size = yMisNum, replace = TRUE)
        }
        impVal <- misPredVal + noiseVec
        return(impVal)
    } else {
        # Use normal assumption
        impVal <- rnorm(length(misPredVal),
                        mean = misPredVal,
                        sd = sqrt(rfObj[["prediction.error"]]))
        return(impVal)
    }
}
