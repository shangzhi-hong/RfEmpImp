#' Identify corresponding observations indexes under the terminal nodes for a
#' random forest model by \code{ranger}
#'
#' @description
#' The observation indexes (row numbers) constituting the terminal node
#' associated with each observation are queried using the \code{ranger} object
#' and the training data.
#' The parameter \code{keep.inbag = TRUE} should be applied to call to
#' \code{ranger}.
#'
#' @details
#' The observations are found based on terminal node IDs. It should be noted
#' that the out-of-bag observations are not present in the indexes.
#'
#' @param obj An R object of class \code{ranger}.
#'
#' @param data Input for training data.
#'
#' @param id.name Use the IDs of the terminal nodes as names for the lists.
#'
#' @param unique.by.id Only return results of unique terminal node IDs.
#'
#' @param ... Other parameters (will be ignored).
#'
#' @return A nested list of length \code{num.trees}.
#'
#' @name query.rf.pred.idx
#'
#' @author Shangzhi Hong
#'
#' @examples
#' data(iris)
#' rfObj <- ranger(
#'     Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'     data = iris, num.trees = 5, keep.inbag = TRUE)
#' outList <- query.rf.pred.idx(rfObj, iris)
#'
#' @export
query.rf.pred.idx <- function(
    obj, data, id.name = FALSE, unique.by.id = FALSE, ...) {
    if (!"ranger" %in% class(obj))
        stop("Input `obj` should be of class `ranger`.")
    if (is.null(obj[["inbag.counts"]]))
        stop(paste0("The in-bag frequencies not found. ",
                "Please set `keep.inbag = TRUE` in call to `ranger`."))
    predObjNode <- predict(obj, data = data, type = "terminalNodes")
    nodeIdMatObsTotal <- predObjNode[["predictions"]]
    outList <- vector(mode = "list", length = ncol(nodeIdMatObsTotal))
    for (i in seq_len(ncol(nodeIdMatObsTotal))) {
        # For each tree
        nodeIdMatObs <- nodeIdMatObsTotal[, i, drop = FALSE]
        obsNum <- nrow(data)
        obsNodeRepMat <-
            nodeIdMatObs[rep(x = seq_len(obsNum), times = obsNum), ,
                         drop = FALSE]
        obsNodeRepMatAux <-
            nodeIdMatObs[rep(x = seq_len(obsNum), each = obsNum), ,
                         drop = FALSE]
        nodeCorrIndMat <- obsNodeRepMat == obsNodeRepMatAux
        inbagFreqMat <- matrix(as.integer(unlist(obj[["inbag.counts"]][[i]])),
                               ncol = 1,
                               byrow = FALSE)
        inbagFreqRepMat <- inbagFreqMat[
            rep(x = seq_len(obsNum), times = obsNum), ,
            drop = FALSE]
        nodeCorrInbag <- nodeCorrIndMat * inbagFreqRepMat
        nodeCorrFreqVec <- rowSums(nodeCorrInbag)
        nodeCorrMat <- matrix(
            nodeCorrFreqVec,
            nrow = obsNum,
            ncol = obsNum,
            byrow = FALSE
        )
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
        if (isTRUE(unique.by.id)) {
            names(idxList) <- nodeIdMatObsTotal[, i, drop = TRUE]
            idxList <- idxList[
                as.character(sort(unique(nodeIdMatObsTotal[, i, drop = TRUE])))]
        } else if (isTRUE(id.name)) {
            names(idxList) <- nodeIdMatObsTotal[, i, drop = TRUE]
        }
        outList[[i]] <- idxList
    }
    return(outList)
}
