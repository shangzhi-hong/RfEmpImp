#' Make Missing Cells in a Data Frame
#'
#' @param df Input complete data frame
#' @param propMiss Proportion of missing cells, default = 0.2
#' @param warnEmptyRow Whether try to warn about empty rows
#'
#' @return A data frame containing NAs
#' @export
gen.mcar <- function(
    df,
    propMiss = 0.2,
    warnEmptyRow = TRUE,
    maxTry = 10) {
    r <- nrow(df)
    c <- ncol(df)
    cellNum <- r * c
    naNum <- ceiling(cellNum * propMiss)
    thisNaLoc <- rep(FALSE, cellNum)
    thisNaLoc[sample(cellNum, size = naNum, replace = FALSE)] <- TRUE
    df[matrix(thisNaLoc, nrow = r, ncol = c)] <- NA
    anyMisRow <- any(rowSums(is.na(df)) == c)
    if (anyMisRow) {
        warning("Generated output contains empty row(s).", call. = FALSE)
    }
    return(df)
}
