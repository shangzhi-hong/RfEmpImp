#' Univariate sampler function for continuous variables for prediction-based
#' imputation, assuming normality for prediction errors of random forest
#'
#' @description
#' Please note that functions with names starting with "mice.impute" are
#' exported to be visible for the mice sampler functions. Please do not call
#' these functions directly unless you know exactly what you are doing.
#'
#' For continuous variables only.
#'
#' This function is for \code{RfPred.Norm}
#' multiple imputation method, adapter for \code{mice} samplers.
#' In the \code{mice()} function, set \code{method = "rfpred.norm"} to call it.
#'
#' The function performs multiple imputation based on normality assumption using
#' out-of-bag mean squared error as the estimate for the variance.
#'
#' @details
#' \code{RfPred.Norm} imputation sampler.
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
#' @param num.trees.cont Number of trees to build for continuous variables.
#'  The default is \code{num.trees = 10}.
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
#' @name mice.impute.rfpred.norm
#'
#' @author Shangzhi Hong
#'
#' @references
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' @examples
#' # Users can set method = "rfpred.norm" in call to mice to use this method
#' data("airquality")
#' impObj <- mice(airquality, method = "rfpred.norm", m = 5,
#' maxit = 5, maxcor = 1.0, eps = 0,
#' remove.collinear = FALSE, remove.constant = FALSE,
#' printFlag = FALSE)
#'
#' @export
mice.impute.rfpred.norm <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cont = 10,
    pre.boot = TRUE,
    num.threads = NULL,
    ...) {
    return(
        mice.impute.rfpred.emp(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.cont = num.trees.cont,
            emp.err.cont = FALSE,
            pre.boot = pre.boot,
            num.threads = num.threads,
            ...
        )
    )
}
