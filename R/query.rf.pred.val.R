#' Identify corresponding observed values for the response variable under the
#' terminal nodes for a random forest model by \code{ranger}
#'
#' @description
#' The observed values (for the response variable) constituting the terminal
#' node associated with each observation are queried using the \code{ranger}
#' object and the training data.
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
#' @name query.rf.pred.val
#'
#' @author Shangzhi Hong
#'
#' @examples
#' data(iris)
#' rfObj <- ranger(
#'     Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'     data = iris, num.trees = 5, keep.inbag = TRUE)
#' outList <- query.rf.pred.val(rfObj, iris)
#'
#' @export
query.rf.pred.val <- function(
    obj, data, id.name = FALSE, unique.by.id = FALSE, ...) {
    predIdxList <- query.rf.pred.idx(
        obj = obj,
        data = data,
        id.name = id.name,
        unique.by.id = unique.by.id)
    respVarName <- all.vars(obj[["call"]])[1]
    obsVec <- data[[respVarName]]
    outList <- mapply(
        FUN = function(inList, obsVec) {
            return(lapply(X = inList, FUN = function(x) obsVec[x]))
        },
        inList = predIdxList,
        MoreArgs = list(obsVec = obsVec),
        SIMPLIFY = FALSE
    )
    return(outList)
}
