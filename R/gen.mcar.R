#' Generate missing (completely at random) cells in a data set
#'
#' @param df Input data frame or matrix.
#'
#' @param prop.na Proportion of generated missing cells. The default is
#' \code{prop.na = 0.2}.
#'
#' @param warn.empty.row Show a warning if empty rows were present in the
#' output data set.
#'
#' @param ... Other parameters (will be ignored).
#'
#' @return A data frame or matrix containing generated missing cells.
#'
#' @name gen.mcar
#'
#' @author Shangzhi Hong
#'
#' @examples
#' data("mtcars")
#' mtcars.mcar <- gen.mcar(mtcars, warn.empty.row = FALSE)
#'
#' @export
gen.mcar <- function(
    df,
    prop.na = 0.2,
    warn.empty.row = TRUE,
    ...) {
    r <- nrow(df)
    c <- ncol(df)
    cellNum <- r * c
    naNum <- ceiling(cellNum * prop.na)
    naLoc <- rep(FALSE, cellNum)
    naLoc[sample.int(cellNum, size = naNum, replace = FALSE)] <- TRUE
    df[matrix(naLoc, nrow = r, ncol = c)] <- NA
    if (warn.empty.row) {
        if (any(rowSums(is.na(df)) == c))
            warning("Generated output contains empty row(s).", call. = FALSE)
    }
    return(df)
}
