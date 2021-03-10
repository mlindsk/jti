## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------

## MAPS
.map_chr     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
.map_int     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
.map_dbl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
.map_lst     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = list(), ...)

## STRINGS
## str_rem <- function(s, pos) {
##   # Vectorized removal of substrings
##   # s: character vector
##   .map_chr(strsplit(s, ""), function(x) {
##     paste0(x[-pos], collapse = "")
##   })
## }

## GRAPHS
as_adj_lst <- function(A, indices = FALSE) {
  # indices: If true, elements are vectors or indices
  #          else the names provided in dimnames
  names_ <- if (!indices) colnames(A) else 1:ncol(A)
  out <- lapply(seq_along(names_), function(r) {
    names_[as.logical(A[, r])]
  })
  names(out) <- names_
  out
}


as_adj_mat <- function(adj) {
  # TODO: Convert to c++ function using arma::Mat
  stopifnot(length(names(adj)) == length(adj))
  names_ <- names(adj)
  N      <- length(names_)
  A      <- matrix(0L, nrow = N, ncol = N, dimnames = list(names_, names_))
  for (d in seq_along(names_)) {
    idx <- match(adj[[d]], names_)
    A[idx, d] <- 1L
    A[d, idx] <- 1L
  }
  A
}

subgraph <- function(x, g) {
  # x: vector of nodes to delete - either character or indices
  if (inherits(g, "matrix")) {
    keepers <- if (inherits(x, "character")) {
      setdiff(dimnames(g)[[1]], x)
    } else {
      setdiff(1:ncol(g), x)
    }
    g <- g[keepers, keepers]
    return(g)
  }
  else if (inherits(g, "list")) {
    g <- if (inherits(x, "character")) {
      g[-match(x, names(g))]
    } else {
      g[-x]
    }
    g <- lapply(g, function(e) {
      rm_idx <- as.vector(stats::na.omit(match(x, e)))
      if (neq_empt_int(rm_idx)) return(e[-rm_idx])
      return(e)
    })
    return(g)
  }
  else {
    stop("g must either be a matrix of an adjacency list.", call. = FALSE)
  }
}

## MISC
push <- function(l, el, name = NULL) {
  # TODO: if el is a named list, we must take this into account
  c(l, structure(list(el), names = name))
}
