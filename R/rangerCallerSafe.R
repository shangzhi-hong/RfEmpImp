#' Remove unnecessary arguments for \code{ranger} function
#'
#' This function serves as an workaround for ranger function.
#  Will let ranger handle unused arguments after v0.12.3.
#'
#' @param ... Parameters to pass down.
#'
#' @return Constructed \code{ranger} object.
rangerCallerSafe <- function(...) {
    params <- list(...)
    validName <- setdiff(names(formals(ranger)), "...")
    params <- params[names(params) %in% validName]
    return(suppressWarnings(do.call(what = ranger, args = params)))
}
