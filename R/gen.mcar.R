#' Generate cells missing completely at random in the dataset
#'
#' @param df Input complete data frame.
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
    thisNaLoc <- rep(FALSE, cellNum)
    thisNaLoc[sample(cellNum, size = naNum, replace = FALSE)] <- TRUE
    df[matrix(thisNaLoc, nrow = r, ncol = c)] <- NA
    if (any(rowSums(is.na(df)) == c)) {
        warning("Generated output contains empty row(s).", call. = FALSE)
    }
    return(df)
}
