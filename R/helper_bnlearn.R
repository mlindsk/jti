# TODO: Export
bnlearn_to_cpts <- function(l) {
  cpts <- lapply(l, function(x) {
    xp <- x$prob
    # Make as_sparta.table instead of this hack
    class(xp) <- c("array", class(xp))
    if (length(dim(xp)) == 1L) {
      xn <- structure(list(dimnames(xp)[[1]]), names = x$node)
      dimnames(xp) <- xn
    }
    xp
  })
}
