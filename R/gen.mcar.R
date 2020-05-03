#' Generate missing (completely at random) cells in the dataset
#'
#' @param df Input data frame.
#'
#' @param prop.na Proportion of missing cells, default = 0.2.
#'
#' @param warn.empty.row Show warnings if empty rows were present.
#'
#' @param ... Other parameters (will be ignored).
#'
#' @return A data frame containing generated missing values.
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
