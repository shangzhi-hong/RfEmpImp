#' Multiple imputation using chained random forests: RfEmp
#'
#' @description
#' \code{RfEmpImp} multiple imputation method for mixed types of variables,
#' and calls corresponding functions based on variable types.
#' Categorical variables should be of type \code{factor} or \code{logical}.
#'
#' For continuous variables, \code{mice.impute.rfpred.emp} is called, performing
#' imputation based on the empirical distribution of out-of-bag
#' prediction errors of random forests.
#'
#' For categorical variables, \code{mice.impute.rfpred.cate} is called,
#' performing imputation based on predicted probabilities.
#'
#' @details
#' \code{RfEmpImp} multiple imputation, the \code{mice.impute.rfemp} calls
#' \code{mice.impute.rfpred.emp} if the variable is numeric,
#' otherwise it calls \code{mice.impute.rfpred.cate}.
#'
#' @param data A data frame or a matrix containing the incomplete data. Missing
#' values should be coded as \code{NA}s.
#'
#' @param num.imp Number of multiple imputations. The default is 5.
#'
#' @param max.iter Number of iterations. The default is 5.
#'
#' @param num.trees Number of trees to build, default to \code{10}.
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
#' @param num.trees.cont Number of trees to build for continuous variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param num.trees.cate Number of trees to build for categorical variables,
#' default to \code{NULL} to use the value of \code{num.trees}.
#'
#' @param print.flag If \code{TRUE}, details will be sent to console.
#'
#' @param ... Other arguments to pass down.
#'
#' @return An object of S3 class \code{mids}.
#'
#' @name imp.rfemp
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#' Hong, Shangzhi, et al. "Multiple imputation using chained random forests"
#' arXiv:2004.14823.
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
#' \donttest{
#' nhanesFix <- nhanes
#'
#' nhanesFix[, c("age", "hyp")] <-
#' lapply(X = nhanes[, c("age", "hyp")], FUN = as.factor)
#'
#' impRfEmpImp <- imp.rfemp(nhanesFix, printFlag = FALSE)
#' }
#'
#' @export
imp.rfemp <- function(
    data,
    num.imp = 5,
    max.iter = 5,
    num.trees = 10,
    alpha.emp = 0.0,
    sym.cont = TRUE,
    pre.boot = TRUE,
    num.trees.cont = NULL,
    num.trees.cate = NULL,
    print.flag = FALSE,
    ...) {
    return(mice(
        data = data,
        method = "rfemp",
        m = num.imp,
        maxit = max.iter,
        num.trees = num.trees,
        alpha.emp = alpha.emp,
        sym.cont = sym.cont,
        pre.boot = pre.boot,
        num.trees.cont = num.trees.cont,
        num.trees.cate = num.trees.cate,
        printFlag = print.flag,
        # Try to avoid the influences of remove.lindep()
        maxcor = 1.0,
        eps = .Machine$double.xmin,
        ...))
}
