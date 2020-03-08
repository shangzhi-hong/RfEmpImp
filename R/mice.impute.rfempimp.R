#' Multiple imputation  based on empirical distribution for categorical
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
#' @param num.trees Number of trees to build, default to \code{10}, overrides
#' \code{num.trees.continuous} and \code{num.trees.categorical}
#'
#' @param num.trees.continuous Number of trees to build for imputing continuous
#' variables, default to \code{10}
#'
#' @param symmetry.continuous Logical indicator, \code{TRUE} for
#' assuming symmetric empirical error distribution,
#' \code{FALSE} for asymmetric empirical error distribution,
#' default to \code{FALSE}. This option is invalid when
#' \code{empirical.error.continuous} is set to \code{FALSE}.
#'
#' @param num.trees.categorical Number of trees to build for imputing
#' categorical variables, default to \code{10}
#'
#' @param use.pred.prob.categorical Logical indicator, \code{TRUE} for
#' imputation based on probabilities of votes, \code{FALSE} for imputation
#' based on majority votes, default to \code{TRUE}
#'
#' @param pre.bootstrap Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014)
#'
#' @param empirical.error.continuous Logical indicator, \code{TRUE} for using
#' empirical error; \code{FALSE}, for assuming normal distribution for the
#' prediction error, the variance equals to overall out-of-bag prediction error,
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
#' The American Statistician just-accepted (2019): 1-20.
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' @examples
#' imp <- mice(nhanes, method = "rfempimp", m = 10, max.iter = 10)
#'
#' @export
mice.impute.rfempimp <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees = 10,
    num.trees.continuous = NULL,
    symmetry.continuous = TRUE,
    num.trees.categorical = NULL,
    use.pred.prob.categorical = TRUE,
    pre.bootstrap = TRUE,
    empirical.error.continuous = TRUE,
    alpha.empirical = 0.0,
    ...
) {
    if (is.numeric(y)) {
        if (is.null(num.trees.continuous)) num.trees.continuous <- num.trees
        rfImpVec <- mice.impute.rfempimp.continuous(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.continuous = num.trees.continuous,
            symmetry.continuous = symmetry.continuous,
            pre.bootstrap = pre.bootstrap,
            ...
        )
    } else {
        if (is.null(num.trees.categorical)) num.trees.categorical <- num.trees
        rfImpVec <- mice.impute.rfempimp.categorical(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.categorical = num.trees.categorical,
            use.pred.prob.categorical = use.pred.prob.categorical,
            pre.bootstrap = pre.bootstrap,
            ...
        )
    }
    return(rfImpVec)
}
