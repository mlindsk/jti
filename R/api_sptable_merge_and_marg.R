#' Merge sparse tables
#'
#' Multiplication, division, addition and subtraction of two sparse tables
#' 
#' @param x A \code{sptable} object
#' @param y A \code{sptable} object
#' @param op Either "*" (multiplication) or "/" (division)
#' @param ... For S3 compatability
#' @seealso \code{\link{\%m*\%}}, \code{\link{\%m/\%}}
#' @examples
#' # Variables in common
#' x1 <- sptable(asia[, 2:5])
#' y1 <- sptable(asia[, c(5, 8, 4)])
#' merge(x1, y1, "*")
#'
#' # No variables in common
#' x2 <- sptable(asia[, 1:2])
#' y2 <- sptable(asia[, 3, drop = FALSE])
#' x2 %m/% y2
#'
#' @export
merge.sptable <- function(x, y, op = "*", ...) {

  stopifnot(op %in% c("*", "/"))# , "+", "-"))
  stopifnot(inherits(y, "sptable")) # Since no double dispatch
  
  if (inherits(x, "unity_sptable") || inherits(y, "unity_sptable")) {
    return(merge_unity_sptable(x, y, op))
  }
  
  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep <- intersect(vx, vy)

  # If no variables in common it is easy
  if (!neq_empt_chr(sep)) {
    # TODO: Put this into a function: merge_sptables_with_empty_separator
    spt <- unlist(lapply(names(x), function(nx_) {
      .names <- paste0(nx_, names(y))
      .vals  <- do.call(op, list(x[nx_, "value"], y[names(y), "value"]))
      structure(.vals, names = .names)
    }))

    spt <- list2env(as.list(spt))
    attr(spt, "vars") <- c(vx, vy)
    class(spt) <- c("sptable", class(spt))
    return(spt)
  }

  posx <- match(sep, vx)
  posy <- match(sep, vy)
  
  cfx  <- .find_cond_configs(x, posx)
  cfy  <- .find_cond_configs(y, posy)
  
  scfx <- split(names(cfx), cfx)
  scfy <- split(names(cfy), cfy)

  # We need to align the tables to have identical names in order to merge correctly
  # - (if !(length(sep) > 1) names in scfx and scfy must agree!
  if (length(sep) > 1L && !identical(names(scfx), names(scfy))) {
    # TODO: put this into a function
    posx_sep    <- structure(seq_along(posx), names = vx[posx])
    posy_sorted <- sort(posy)
    posy_sep    <- structure(seq_along(posy_sorted), names = vy[posy_sorted])
    posy_new    <- posy_sep[names(posx_sep)]
    scfy        <- .reposition_names(scfy, posy_new)
  }

  # Those not in sc_sep are are not present in the sparse representations
  # - this is what kills the `+` and `-` operations. May be implemented in the future
  sc_sep <- intersect(names(scfx), names(scfy))
  scfx   <- scfx[sc_sep]
  scfy   <- scfy[sc_sep]


  # TODO: Can we parallelize this?
  spt <- lapply(sc_sep, function(z) {

    scfx_z <- scfx[[z]]
    scfy_z <- scfy[[z]]
    
    xz <- x[scfx_z]
    yz <- y[scfy_z]
    
    xn_ <- names(xz)
    yn_ <- names(yz)
    yn_rem <- str_rem(names(yz), posy)
    
    unlist(lapply(xn_, function(z) {
      .names <- paste0(z, yn_rem)
      .vals  <- do.call(op, list(xz[z, "value"], yz[yn_, "value"]))
      structure(.vals, names = .names)
    }))
  })

  spt <- list2env(as.list(unlist(spt, recursive = FALSE)))
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable", class(spt))
  return(spt)
}

#' Sparse table algebra
#'
#' Multiplication, division, etc. for sparse tables
#' 
#' @param x,y \code{sptable} objects
#' @aliases %m*% %m/% %m+% %m-%
#'
#' @rdname merge
#' @export
"%m*%" <- function(x, y) merge(x, y, op = "*")

#' @rdname merge
#' @export
"%m/%" <- function(x, y) merge(x, y, op = "/")

## #' @rdname merge
## #' @export
## "%m+%" <- function(x, y) merge(x, y, op = "+")

## #' @rdname merge
## #' @export
## "%m-%" <- function(x, y) merge(x, y, op = "-")

#' Marginalize
#'
#' @param p A \code{sptable} object
#' @param s The variables to be marginalized out
#' @param flow Either "sum" or "max"
#' @examples
#' p <- sptable(asia[, 3:5])
#' p
#' marginalize(p, "L")
#' @export
marginalize <- function(p, s, flow = "sum") UseMethod("marginalize")


#' @rdname marginalize
#' @export
marginalize.sptable <- function(p, s, flow = "sum") {

  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")

  v <- attr(p, "vars")
  if (anyNA(match(s, v))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- .find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  spt <- list2env(lapply(scf, function(e) {
    # if (flow == "sum") sum(unlist(mget(e, envir = p))) else max(unlist(mget(e, envir = p)))
    if (flow == "sum") sum(p[e]) else max(p[e])
  }))

  class(spt) <- c("sptable", "environment")
  attr(spt, "vars") <- marg_vars
  return(spt)
}
