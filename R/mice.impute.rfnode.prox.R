#' @rdname mice.impute.rfnode
#' @order 3
#'
#' @export
mice.impute.rfnode.prox <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees = 10,
    pre.boot = TRUE,
    obs.eq.prob = FALSE,
    ...
    ) {
    return(
        mice.impute.rfnode(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.node = num.trees,
            pre.boot = pre.boot,
            use.node.cond.dist = FALSE,
            obs.eq.prob = obs.eq.prob,
            do.sample = TRUE,
            ...)
    )
}
