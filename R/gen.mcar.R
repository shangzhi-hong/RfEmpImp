#' Generate missing cells in the dataset
#'
#' @param df Input complete data frame
#' @param prop.na Proportion of missing cells, default = 0.2
#'
#' @return A data frame containing NAs
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
    thisNaLoc <- rep(FALSE, cellNum)
    thisNaLoc[sample(cellNum, size = naNum, replace = FALSE)] <- TRUE
    df[matrix(thisNaLoc, nrow = r, ncol = c)] <- NA
    if (any(rowSums(is.na(df)) == c)) {
        warning("Generated output contains empty row(s).", call. = FALSE)
    }
    return(df)
}
