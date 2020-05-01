#' Multiple imputation using chained random forests: RfPred.Norm
#'
#' @description
#' For continuous variables only.
#'
#' This function is for \code{RfPred.Norm}
#' multiple imputation method, adapter for \code{mice} samplers.
#' In the \code{mice()} function, set \code{method = "rfpred.norm"} to call it.
#'
#' The function performs multiple imputation based on normality assumption using
#' out-of-bag mean squared error as the estimate for variance.
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
#' @name mice.impute.rfpred.norm
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' @examples
#' \donttest{
#' impRfPredEmp <- mice(nhanes, method = "rfpred.norm", m = 10,
#' max.iter = 10, maxcor = 1.0, printFlag = FALSE)#' }
#'
#' @export
mice.impute.rfpred.norm <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cont = 10,
    pre.boot = TRUE,
    ...) {
    mice.impute.rfpred.emp(
        y = y,
        ry = ry,
        x = x,
        wy = wy,
        num.trees.cont = 10,
        sym.cont = FALSE,
        pre.boot = pre.boot,
        emp.err.cont = FALSE,
        alpha.emp = 1.0,
        ...
    )
}
