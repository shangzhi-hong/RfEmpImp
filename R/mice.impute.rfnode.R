#' Multiple imputation based on predicting nodes of random forest
#'
#' @description
#' \code{RfNode} imputation methods, adapter for \code{mice} samplers.
#' These functions can be called by the \code{mice} sampler functions.
#'
#' \code{mice.impute.rfnode.cond} is for imputation using the conditional formed
#' by the predicting nodes of random forests. To use this function, set
#' \code{method = "rfnode.cond"} in \code{mice} function.
#'
#' \code{mice.impute.rfnode.prox} is for imputation based on proximity measures
#' from random forests, and provides functionality similar to
#' \code{mice.impute.rf}. To use this function, set
#' \code{method = "rfnode.prox"} in \code{mice} function.
#'
#' \code{mice.impute.rfnode} is the main function for performing imputation, and
#' both \code{mice.impute.rfnode.cond} and \code{mice.impute.rfnode.prox} call
#' this function. By default, \code{mice.impute.rfnode} works like
#' \code{mice.impute.rfnode.cond}.
#'
#' @details
#' Users can get more flexibility from \code{mice.impute.rfnode} function,
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
#' @param num.trees.node Number of trees to build for imputing continuous
#' variables, default to \code{10}
#'
#' @param pre.boot Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014).
#'
#' @param use.node.cond.dist If \code{TRUE}, use conditional distribution formed
#' by predicting nodes of random forest (out-of-bag observations were excluded);
#' if \code{FALSE}, use proximity-based imputation.
#'
#' @param obs.eq.prob If \code{TRUE}, the candidate observations will be sampled
#' with equal probability.
#'
#' @param do.sample If \code{TRUE}, draw samples for missing observations;
#' if \code{FALSE}, the corresponding observations numbers will be returned.
#' For testing purposes, and WILL CAUSE ERRORS for the \code{mice} sampler
#' function.
#'
#' @param num.threads Number of threads. Default to \code{NULL} to use all the
#' processors available.
#'
#' @param ... Other arguments to pass down
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @name mice.impute.rfnode
#' @order 1
#'
#' @references
#' Hong, Shangzhi, et al. "Multiple imputation using chained random forests"
#' arXiv:2004.14823.
#'
#' Doove, Lisa L., Stef Van Buuren, and Elise Dusseldorp.
#' "Recursive partitioning for missing data imputation in the presence of
#' interaction effects."
#' Computational Statistics & Data Analysis 72 (2014): 92-104.
#'
#' @examples
#' \donttest{
#' nhanesFix <- nhanes
#'
#' nhanesFix[, c("age", "hyp")] <-
#' lapply(X = nhanes[, c("age", "hyp")], FUN = as.factor)
#'
#' # Using "rfnode.cond"
#' impRfNodeCond <- mice(nhanesFix, method = "rfnode.cond", m = 10,
#' maxit = 10, maxcor = 1.0, printFlag = FALSE)
#'
#' # Using "rfnode.prox"
#' impRfNodeProx <- mice(nhanesFix, method = "rfnode.prox", m = 10,
#' maxit = 10, maxcor = 1.0, printFlag = FALSE)
#' }
#'
#' @export
mice.impute.rfnode <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.node = 10,
    pre.boot = TRUE,
    use.node.cond.dist = TRUE,
    obs.eq.prob = FALSE,
    do.sample = TRUE,
    num.threads = NULL,
    ...) {
    if (is.null(wy)) wy <- !ry
    if (isTRUE(pre.boot)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    xMis <- x[wy, , drop = FALSE]
    # Output in-bag list when using conditional distribution
    # TODO: Add `...` back to ranger when updates available
    rfObj <- ranger(x = xObs,
                    y = yObs,
                    num.trees = num.trees.node,
                    keep.inbag = use.node.cond.dist,
                    num.threads = num.threads)
    # Get Nodes for training and test set
    nodeObjMis <- predict(rfObj, data = xMis, type = "terminalNodes")
    nodeObjObs <- predict(rfObj, data = xObs, type = "terminalNodes")
    nodeIdMatObs <- nodeObjObs[["predictions"]]
    nodeIdMatMis <- nodeObjMis[["predictions"]]
    obsNum <- nrow(xObs)
    misNum <- nrow(xMis)
    # Repeated matrix of IDs of non-missing observations, repeated by whole
    # row: obsNum * misNum; column: num.tree
    obsNodeRepMat <- nodeIdMatObs[rep(x = seq_len(obsNum), times = misNum), ,
                                  drop = FALSE]
    # Repeated matrix of IDs of missing observations, repeated by each
    misNodeRepMat <- nodeIdMatMis[rep(x = seq_len(misNum), each = obsNum), ,
                                  drop = FALSE]
    # Observations tagged for being under the same node
    nodeCorrIndMat <- obsNodeRepMat == misNodeRepMat
    if (use.node.cond.dist) {
        # Use the conditional distribution by nodes excluding the OOB
        inbagFreqMat <- matrix(
            as.integer(unlist(rfObj[["inbag.counts"]])),
            ncol = rfObj[["num.trees"]],
            byrow = FALSE)
        inbagFreqRepMat <- inbagFreqMat[
            rep(x = seq_len(obsNum), times = misNum), ,
            drop = FALSE]
        nodeCorrInbag <- nodeCorrIndMat * inbagFreqRepMat
        # Summing over trees
        nodeCorrFreqVec <- rowSums(nodeCorrInbag)
    } else {
        # Vector, 2nd index is observed obs
        nodeCorrFreqVec <- rowSums(nodeCorrIndMat)
    }
    nodeCorrMat <- matrix(nodeCorrFreqVec,
                          nrow = obsNum,
                          ncol = misNum,
                          byrow = FALSE)
    # Observations will be sampled with equal probability
    if (obs.eq.prob) nodeCorrMat <- (nodeCorrMat > 0) * 1L
    # Non-zero location for row number, and the value for sampling weight
    # Sample from matched observations, column-wise for each missing observation
    if (do.sample) {
        impIdx <- apply(
            X = nodeCorrMat,
            MARGIN = 2,
            FUN = function(vec) {
                usedIdx <- which(vec > 0)
                repFreq <- vec[usedIdx]
                sampleVec <- rep(usedIdx, times = repFreq)
                return(sample(x = sampleVec, size = 1))
            }
        )
        return(yObs[impIdx])
    } else {
        idxList <- apply(
            X = nodeCorrMat,
            MARGIN = 2,
            FUN = function(vec) {
                usedIdx <- which(vec > 0)
                repFreq <- vec[usedIdx]
                sampleVec <- rep(usedIdx, times = repFreq)
                return(sampleVec)
            }
        )
        return(idxList)
    }
}

############################################################################
# If you are reading this, please note:
#
# 1. This script aims to find the corresponding nodes used for predictions
# with better efficiency and consistency by using matrix manipulations
# instead of joining tables (like "quantforesterror" in R package
# "forestError") or equality testings (like "mice.rf" in R package "mice")
#
# 2. The nodes is found by corresponding node IDs, which is different from
# previous implementations like in Doove et al., that used equality testings
# for double precision values and one-by-one way for constructing RF
############################################################################
