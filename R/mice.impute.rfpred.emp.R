#' Univariate sampler function for continuous variables using the empirical
#' error distributions
#'
#' @description
#' Please note that functions with names starting with "mice.impute" are
#' exported to be visible for the mice sampler functions. Please do not call
#' these functions directly unless you know exactly what you are doing.
#'
#' For continuous variables only.
#'
#' This function is for \code{RfPred.Emp} multiple imputation method, adapter
#' for \code{mice} samplers. In the \code{mice()} function, set
#' \code{method = "rfpred.emp"} to call it.
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
#' @param num.trees Number of trees to build. The default is
#' \code{num.trees = 10}.
#'
#' @param num.trees.cont Number of trees to build for continuous variables.
#'  The default is \code{num.trees = 10}.
#'
#' @param emp.err.cont If \code{TRUE}, the empirical distribution of out-of-bag
#' prediction errors will be used for constructing conditional distributions;
#' if \code{FALSE}, for normality will be assumed for the distribution for the
#' prediction errors, the variance estimate equals to overall out-of-bag
#' prediction error, i.e. out-of-bag mean squared error (see Shah et al. 2014).
#'
#' @param alpha.emp The "significance level" for the empirical distribution of
#' out-of-bag prediction errors, can be used for prevention for outliers
#' (useful for highly skewed variables).
#' For example, set alpha = 0.05 to use 95\% confidence level.
#' The default is \code{alpha.emp = 0.0}, and the empirical distribution of
#' out-of-bag prediction errors will be kept intact.
#' This option is invalid when \code{emp.err.cont = FALSE}.
#'
#' @param sym.dist If \code{TRUE}, the empirical distribution of out-of-bag
#' prediction errors will be assumed to be symmetric; if \code{FALSE}, the
#' empirical distribution will be kept intact. The default is
#' \code{sym.dist = TRUE}.
#' This option is invalid when \code{emp.err.cont = FALSE}.
#'
#' @param pre.boot If \code{TRUE}, bootstrapping prior to imputation will be
#' performed to perform 'proper' multiple imputation, for accommodating sampling
#' variation in estimating population regression parameters
#' (see Shah et al. 2014).
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
#' @name mice.impute.rfpred.emp
#'
#' @author Shangzhi Hong
#'
#' @references
#' Hong, Shangzhi, et al. "Multiple imputation using chained random forests."
#' Preprint, submitted April 30, 2020. https://arxiv.org/abs/2004.14823.
#'
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
#' # Users can set method = "rfpred.emp" in call to mice to use this method
#' data("airquality")
#' impObj <- mice(airquality, method = "rfpred.emp", m = 5,
#' maxit = 5, maxcor = 1.0, eps = 0, printFlag = FALSE)
#'
#' @export
mice.impute.rfpred.emp <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cont = 10,
    sym.dist = TRUE,
    emp.err.cont = TRUE,
    alpha.emp = 0.0,
    pre.boot = TRUE,
    num.threads = NULL,
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
    # TODO: Add `...` back to ranger, and add `suppressWarnings`
    # after release of v0.12.3
    rfObj <- ranger(
        x = xObs,
        y = yObs,
        oob.error = TRUE,
        num.trees = num.trees.cont,
        num.threads = num.threads)
    misPredVal <- predictions(predict(rfObj, xMis))
    if (emp.err.cont) {
        # Get empirical OOB prediction errors
        oobErrEmp <-  yObs - rfObj[["predictions"]]
        # To fix "NaN"s in OOB error due to small tree number
        oobErrEmp <- oobErrEmp[!is.na(oobErrEmp)]
        if (isTRUE(sym.dist)) {
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
