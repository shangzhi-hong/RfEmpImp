#' @rdname mice.impute.rfnode
#' @order 2
#'
#' @export
mice.impute.rfnode.cond <- function(
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
            use.node.cond.dist = TRUE,
            obs.eq.prob = obs.eq.prob,
            do.sample = TRUE,
            ...)
    )
}
