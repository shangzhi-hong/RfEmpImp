#' @rdname mice.impute.rfemp
#' @order 2
#'
#' @export
mice.impute.rfemp.cont <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.cont = 10,
    sym.cont = TRUE,
    pre.boot = TRUE,
    emp.err.cont = TRUE,
    alpha.emp = 1.0,
    ...
    ) {
    if (is.null(wy)) wy <- !ry
    yMisNum <- sum(wy)
    if (isTRUE(pre.boot)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    xMis <- x[wy, , drop = FALSE]
    rangerObj <- ranger(
        x = xObs,
        y = yObs,
        oob.error = TRUE,
        num.trees = num.trees.cont)
    misPredVal <- predictions(predict(rangerObj, xMis))
    if (emp.err.cont) {
        # Get OOB error
        oobErrEmp <-  yObs - rangerObj[["predictions"]]
        # To fix "NaN"s in OOB error due to small tree number
        oobErrEmp <- oobErrEmp[!is.na(oobErrEmp)]
        if (isTRUE(sym.cont)) {
            oobErrEmpAbs <- abs(oobErrEmp)
            if (isTRUE(alpha.emp > 0 && alpha.emp < 1)) {
                oobErrorEmpHi <- quantile(oobErrEmpAbs,
                                          probs = (1 - alpha.emp),
                                          na.rm = TRUE,
                                          names = FALSE)
                oobErrEmpAbs <- oobErrEmpAbs[oobErrEmpAbs < oobErrorEmpHi]
            }
            noiseAbs <- sample(x = oobErrEmpAbs, size = yMisNum, replace = TRUE)
            signVec <- sample(x = c(1L, -1L), size = yMisNum, replace = TRUE)
            noiseVec <- signVec * noiseAbs
        } else {
            if (isTRUE(alpha.emp > 0 && alpha.emp < 1)) {
                oobErrorEmpLimit <- quantile(
                    oobErrEmp,
                    probs = c((alpha.emp / 2),
                              (1 - alpha.emp / 2)),
                    na.rm = TRUE,
                    names = FALSE)
                oobErrEmp <- oobErrEmp[
                    oobErrEmp > oobErrorEmpLimit[1] &
                    oobErrEmp < oobErrorEmpLimit[2]]
            }
            noiseVec <- sample(
                x = oobErrEmp,
                size = yMisNum,
                replace = TRUE)
        }
        impVal <- misPredVal + noiseVec
    } else {
        # Use overall out-of-bag prediction error
        impVal <- rnorm(length(misPredVal),
                        mean = misPredVal,
                        sd = sqrt(rangerObj[["prediction.error"]]))
    }
    return(impVal)
}
