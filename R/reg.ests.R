#' Get regression estimates for pooled object
#'
#' @description
#' Get the estimates with corresponding confidence intervals after pooling.
#'
#' @param obj Pooled object from function \code{pool()}.
#'
#' @param ... Other parameters to pass down.
#'
#' @return A data frame containing coefficient estimates and corresponding
#' confidence intervals.
#'
#' @export
reg.ests <- function(obj, ...) {
    df <- summary(obj, conf.int = TRUE, ...)
    colnames(df)[which(endsWith(colnames(df), "%"))] <- c("conf.low", "conf.hi")
    return(df)
}
