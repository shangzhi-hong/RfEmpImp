#' Multiple imputation using chained random forests: RfEmp
#'
#' @description
#' \code{RfEmpImp} multiple imputation method, adapter for \code{mice} samplers.
#' These functions can be called by the \code{mice} sampler function. In the
#' \code{mice()} function, set \code{method = "rfemp"} to call it.
#'
#' \code{mice.impute.rfemp} is for mixed types of variables, and calls
#' corresponding functions based on variable types. Categorical variables
#' should be of type \code{factor} or \code{logical}.
#'
#' For continuous variables, \code{mice.impute.rfpred.emp} is called, performing
#' imputation based on the empirical distribution of out-of-bag
#' prediction errors of random forests.
#'
#' For categorical variables, \code{mice.impute.rfpred.cate} is called,
#' performing imputation based on predicted probabilities.
#'
#' @details
#' \code{RfEmpImp} imputation sampler, the \code{mice.impute.rfemp} calls
#' \code{mice.impute.rfpred.emp} if the variable is numeric,
#' otherwise it calls \code{mice.impute.rfpred.cate}.
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
#' @param num.trees.cate Number of trees to build for categorical variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param use.pred.prob.cate Logical, \code{TRUE} for assigning categories based
#' on predicted probabilities, \code{FALSE} for imputation based on majority
#' votes, default to \code{TRUE}.
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
#' @name mice.impute.rfemp
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
#' impRfEmpImp <- mice(nhanes, method = "rfemp", m = 10,
#' max.iter = 10, maxcor = 1.0, printFlag = FALSE)
#' }
#'
#' @export
mice.impute.rfemp <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees = 10,
    num.trees.cont = NULL,
    emp.err.cont = TRUE,
    alpha.emp = 0.0,
    sym.cont = TRUE,
    num.trees.cate = NULL,
    use.pred.prob.cate = TRUE,
    pre.boot = TRUE,
    ...) {
    if (is.numeric(y)) {
        return(
            mice.impute.rfpred.emp(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cont = ifelse(is.null(num.trees.cont),
                                        num.trees,
                                        num.trees.cont),
                sym.cont = sym.cont,
                pre.boot = pre.boot,
                ...
            )
        )
    } else {
        return(
            mice.impute.rfpred.cate(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.cate = ifelse(is.null(num.trees.cate),
                                        num.trees,
                                        num.trees.cate),
                use.pred.prob.cate = use.pred.prob.cate,
                pre.boot = pre.boot,
                ...
            )
        )
    }
}
