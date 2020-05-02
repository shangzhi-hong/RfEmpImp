#' Multiple imputation using chained random forests and conditional
#' distributions formed by predicting nodes
#'
#' @description
#' \code{RfEmpImp} multiple imputation method for mixed types of variables,
#' using conditional distribution formed by predicting nodes of random forest
#' (out-of-bag observations will be excluded).
#'
#' @details
#' \code{imp.rfnode.cond} multiple imputation, for missing observations, the
#' non-missing observations used for imputation will be found by the observations
#' included in the predicting nodes in the random trees.
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
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' multiple imputation, i.e. accommodating sampling variation in estimating
#' population regression parameters (see Shah et al. 2014).
#' It should be noted that if \code{TRUE}, this option is in effect even if the
#' number of trees is set to one.
#'
#' @param print.flag If \code{TRUE}, details will be sent to console.
#'
#' @param ... Other arguments to pass down.
#'
#' @return An object of S3 class \code{mids}.
#'
#' @name imp.rfnode.cond
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
#' nhanesFix[, c("age", "hyp")] <- lapply(nhanes[, c("age", "hyp")], as.factor)
#' impRfEmpImp <- imp.rfnode.cond(nhanesFix, printFlag = FALSE)
#' }
#'
#' @export
imp.rfnode.cond <- function(
    data,
    num.imp = 5,
    max.iter = 5,
    num.trees = 10,
    pre.boot = TRUE,
    print.flag = FALSE,
    ...) {
    return(mice(
        data = data,
        method = "rfnode",
        m = num.imp,
        maxit = max.iter,
        num.trees.node = num.trees,
        pre.boot = pre.boot,
        use.node.cond.dist = TRUE,
        obs.eq.prob = FALSE,
        do.sample = TRUE,
        printFlag = print.flag,
        # Try to avoid the influences of remove.lindep()
        maxcor = 1.0,
        eps = .Machine$double.xmin,
        ...))
}
