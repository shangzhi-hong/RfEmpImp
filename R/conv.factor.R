#' Convert variables to factors
#'
#' @param data Input data frame.
#'
#' @param convNames Names of variable to convert, the default is
#' \code{convNames = NULL}.
#'
#' @param exceptNames Names of variables to be excluded from conversion, the
#' default is \code{convNames = NULL}.
#'
#' @param uniqueNum Unique values in the variables to be converted to factors,
#' the default is \code{uniqueNum = 5}.
#'
#' @return A data frame of converted variables.
#'
#' @examples
#' nhanes.fix <- conv.factor(data = nhanes, convNames = c("age", "hyp"))
#'
#' @export
conv.factor <- function(
    data,
    convNames = NULL,
    exceptNames = NULL,
    uniqueNum = 5) {
    allVarNames <- colnames(data)
    if (!is.null(convNames) && !is.null(exceptNames)) {
        convNames <- setdiff(unique(convNames), unique(exceptNames))
        convNames <- convNames[convNames %in% allVarNames]
    } else if (!is.null(exceptNames)) {
        convNames <- setdiff(allVarNames, unique(exceptNames))
    } else if (!is.null(convNames)) {
        convNames <- unique(convNames)
        convNames <- convNames[convNames %in% allVarNames]
    } else {
        convNames <- allVarNames[sapply(
            X = data, FUN = function(x) length(unique(x))) >= uniqueNum]
    }
    data[, convNames] <- lapply(
        X = data[, convNames , drop = FALSE],
        FUN = as.factor)
    return(data)
}
