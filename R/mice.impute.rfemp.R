#' Univariate sampler function for mixed types of variables for
#' prediction-based imputation, using empirical distribution of out-of-bag
#' prediction errors and predicted probabilities of random forests
#'
#' @description
#' Please note that functions with names starting with "mice.impute" are
#' exported to be visible for the mice sampler functions. Please do not call
#' these functions directly unless you know exactly what you are doing.
#'
#' \code{RfEmpImp} multiple imputation method, adapter for \code{mice} samplers.
#' These functions can be called by the \code{mice} sampler function. In the
#' \code{mice()} function, set \code{method = "rfemp"} to use the \code{RfEmp}
#' method.
#'
#' \code{mice.impute.rfemp} is for mixed types of variables, and it calls
#' corresponding functions according to variable types. Categorical variables
#' should be of type \code{factor} or \code{logical} etc.
#'
#' For continuous variables, \code{mice.impute.rfpred.emp} is called, performing
#' imputation based on the empirical distribution of out-of-bag prediction
#' errors of random forests.
#'
#' For categorical variables, \code{mice.impute.rfpred.cate} is called,
#' performing imputation based on predicted probabilities.
#'
#' @details
#' \code{RfEmpImp} imputation sampler, the \code{mice.impute.rfemp} calls
#' \code{mice.impute.rfpred.emp} if the variable \code{is.numeric} is
#' \code{TRUE}, otherwise it calls \code{mice.impute.rfpred.cate}.
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
#' @param alpha.emp The "significance level" for empirical distribution of
#' prediction errors, can be used for prevention for outliers (useful for highly
#' skewed variables). For example, set alpha = 0.05 to use 95\% confidence level
#' for empirical distribution of prediction errors.
#' Default is \code{0.0}, and the empirical error distribution is kept intact.
#'
#' @param sym.dist If \code{TRUE}, the empirical distribution of out-of-bag
#' prediction errors will be assumed to be symmetric; if \code{FALSE}, the
#' empirical distribution will be kept intact. The default is
#' \code{sym.dist = TRUE}.
#' This option is invalid when \code{emp.err.cont} is set to \code{FALSE}.
#'
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' multiple imputation, i.e. accommodating sampling variation in estimating
#' population regression parameters (see Shah et al. 2014).
#' It should be noted that if \code{TRUE}, this option is in effect even if the
#' number of trees is set to one.
#'
#' @param num.trees.cont Number of trees to build for continuous variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param num.trees.cate Number of trees to build for categorical variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param ... Other arguments to pass down.
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}.
#'
#' @name mice.impute.rfemp
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
#' # Prepare data: convert categorical variables to factors
#' nhanes.fix <- nhanes
#' nhanes.fix[, c("age", "hyp")] <- lapply(nhanes[, c("age", "hyp")], as.factor)
#' # This function is exported to be visible to the mice sampler functions, and
#' # users can set method = "rfemp" in call to mice to use this function.
#' # Users are recommended to use the imp.rfemp function instead:
#' impObj <- mice(nhanes.fix, method = "rfemp", m = 5,
#' maxit = 5, maxcor = 1.0, eps = 0, printFlag = FALSE)
#'
#' @export
mice.impute.rfemp <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees = 10,
    alpha.emp = 0.0,
    sym.dist = TRUE,
    pre.boot = TRUE,
    num.trees.cont = NULL,
    num.trees.cate = NULL,
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
                sym.dist = sym.dist,
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
                use.pred.prob.cate = TRUE,
                pre.boot = pre.boot,
                ...
            )
        )
    }
}
