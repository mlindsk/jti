## ---------------------------------------------------------
##                 EXPORTED FUNCTIONS
## ---------------------------------------------------------
#' Sparse table
#'
#' A sparse contingency table representation of a matrix.
#'
#' @param x Character matrix
#' @seealso \code{\link{as_parray}}
#' @examples
#' sptable(as.matrix(asia[, 1:3]))
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x), !is.null(colnames(x)))
  sptab <- sptab_(x)
  class(sptab) <- c("sptable", class(sptab))
  return(sptab)
}

#' Conditional probability table
#'
#' Returns a sparse conditional probability table conditioned on variables in \code{b}.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param y Character vector with variables to condition on
#' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{sptable} with no conditioning variables, i.e. the marginal table.
#' @return A \code{sptable} object
#' @seealso \code{\link{sptable}}
#' @examples
#' sp  <- sptable(as.matrix(asia[, 1:3]))
#' psp <- as_parray(sp, c("S", "T"))
#' sum(psp) # Since (S,T) have 4 configurations
#' @export
as_parray <- function(x, y = NULL) UseMethod("as_parray")


#' @rdname as_sptable
#' @export
as_parray.sptable <- function(x, y = NULL) {
  stopifnot(inherits(x, "sptable"))
  if (is.null(y)) {
    xs <- x / sum(x)
    class(xs) <- class(x)
    return(xs)
  }
  pos  <- match(y, attr(x, "vars"))
  conf <- .find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)
  parr <- unlist(unname(lapply(conditional_list, function(e) {
    x[e] / sum(x[e])
  })))
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- class(x)
  return(parr)
}


#' Merge sparse tables
#'
#' Multiplication or division of two sparse tables
#' 
#' @param x A \code{sptable} object
#' @param y A \code{sptable} object
#' @param op Either "*" (multiplication), "/" (division), "+" (addition) or "-" (subtraction)
#' @param validate Logical indicating wether or not it should be checked if the names
#' of \code{x} and \code{y} are valid. The names are restricted to be one character long
#' @param ... Not used. For S3 compatability
#' @examples
#'
#' # Variables in common
#' # -------------------
#' x1 <- sptable(as.matrix(asia[, 2:5]))
#' y1 <- sptable(as.matrix(asia[, c(5, 8, 4)]))
#' merge(x1, y1, "/")
#'
#' # No variables in common
#' x2 <- sptable(as.matrix(asia[, 1:2]))
#' y2 <- sptable(as.matrix(asia[, 3, drop = FALSE]))
#' merge(x2, y2, "/")
#'
#' # Names not on correct form
#' z  <- chickwts[, 2, drop = FALSE]
#' x3 <- sptable(as.matrix(z))
#' merge(x3, x3)
#'
#' # Correcting the names
#' z_new <- to_single_chars(z)
#' x4    <- sptable(as.matrix(z_new))
#' x4
#' 
#' @export
merge.sptable <- function(x, y, op = "*", validate = TRUE, ...) {

  stopifnot(op %in% c("*", "/", "+", "-"))
  
  if (inherits(x, "unity_table") || inherits(y, "unity_table")) {
    return(merge_unity(x, y))
  }
  
  if (validate) {
    nvars_x <- length(attr(x, "vars"))
    nvars_y <- length(attr(y, "vars"))
    msg1 <- "One or more names of" 
    msg2 <- "has nchar(name) > 1 which is not allowed."
    for (name in names(x)) if (nchar(name) > nvars_x) stop(paste(msg1, "x", msg2))
    for (name in names(y)) if (nchar(name) > nvars_y) stop(paste(msg1, "y", msg2))
  }
  
  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep <- intersect(vx, vy)

  # If no variables in common it is easy
  if (!neq_empt_chr(sep)) {
    # TODO: Put this into a function: merge_tables_with_empty_separator
    spt <- lapply(seq_along(y), function(i) {
      yi <- y[i]
      structure(do.call(op, list(x, yi)),
        names = paste(names(x), names(y[i]), sep = "")
      )
    })
    spt <- unlist(spt)
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
  
  # Those not in sc_sep are structural zeroes!
  sc_sep <- intersect(names(scfx), names(scfy))
  scfx   <- scfx[sc_sep]
  scfy   <- scfy[sc_sep]

  # TODO: Put this into a function
  spt <- lapply(sc_sep, function(z) {

    scfx_z <- scfx[[z]]
    scfy_z <- scfy[[z]]

    xz <- x[scfx_z]
    yz <- y[scfy_z]

    res <- vector("double", length = length(xz) * length(yz))
    res_names <- vector("character", length = length(xz) * length(yz))

    iter <- 1L
    for (i in seq_along(xz)) {
      for (j in seq_along(yz)) {
        xzi <- xz[i]
        yzj <- yz[j]

        yzj_name <- names(yzj)
        new_name <- paste0(names(xzi), str_rem(yzj_name, posy), collapse = "")

        res[iter] <- if (isTRUE(all.equal(yzj, 0))) 0 else do.call(op, list(xzi, yzj))
        res_names[iter] <- new_name
        iter <- iter + 1L
      }
    }
    structure(res, names = res_names)
  })

  spt <- unlist(spt)
  
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable", class(spt))
  
  return(spt)
}

#' Marginalize
#'
#' @param p A \code{sptable} object
#' @param s The variables to be marginalized out
#' @param flow Either "sum" or "max" where "max"
#' @examples
#' p <- sptable(as.matrix(asia[, 3:5]))
#' p
#' marginalize(p, "L")
#' @export
marginalize <- function(p, s, flow = "sum") UseMethod("marginalize")

#' @rdname marginalize
#' @export
marginalize.sptable <- function(p, s, flow = "sum") {

  # TODO: Fix!
  #  - Just make all the combinations from attr(p, "lvls") and proceed as below?
  if (inherits(p, "unity_table")) stop("Not implemented yet. Fix!")

  v <- attr(p, "vars")
  
  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")
  if (any(is.na(match(s, v)))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- .find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  ## ---------------------------------------------------------
  ## penv <- new.env()
  ## for (k in seq_along(p)) {
  ##   penv[[names(p)[k]]] <- as.numeric(p[k])
  ## }
  ## head(ls(envir = penv))
  ## spt <- lapply(scf, function(e) {
  ##   if (flow == "sum") sum(unlist(mget(e, envir = penv))) else max(penv[[e]])
  ## })
  ## ---------------------------------------------------------
  
  spt <- lapply(scf, function(e) {
    # TODO: Slow because we must "lookup" p[e] !!!
    #  - maybe we make "[.sptable" faster!
    if (flow == "sum") sum(p[e]) else max(p[e])
  })
  
  spt <- unlist(spt)
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable", class(spt))
  return(spt)
}


## ---------------------------------------------------------
##                     SANDBOX
## ---------------------------------------------------------
## x1 <- sptable(as.matrix(asia[, 2:5]))
## y1 <- sptable(as.matrix(asia[, c(5, 8, 4, 1)]))
## y1[1:length(y1)] <- 1L
## z1 <- structure(numeric(0), class = c("sptable", "integer"))
## ny <- c("n", "y")
## attr(z1, "lvls") <- list(B = ny, D = ny, L = ny, A = ny)
## attr(z1, "vars") <- names(attr(z1, "lvls"))
## class(z1) <- c("unity_table", class(z1))


## microbenchmark::microbenchmark(
##   merge(x1, y1),
##   merge_unity(x1, z1)  
## )


## x2 <- sptable(as.matrix(derma[, 1:10]))
## y2 <- sptable(as.matrix(derma[, 6:15]))
## z2 <- structure(numeric(0), class = c("sptable", "integer"))
## attr(z2, "lvls") <- lapply(derma[, 6:15], unique)
## attr(z2, "vars") <- names(attr(z2, "lvls"))
## class(z2) <- c("unity_table", class(z2))


## microbenchmark::microbenchmark(
##   merge(x2, y2),
##   merge_unity(x2, z2)  
## )


## x <- structure(numeric(0), class = c("unity_table","sptable", "integer"))
## nx <- c("0", "1", "2")
## attr(x, "lvls") <- list(A = nx, B = "0", C = c("2", "0"))
## attr(x, "vars") <- names(attr(x, "lvls"))

## y <- structure(numeric(0), class = c("unity_table","sptable", "integer"))
## attr(y, "lvls") <- list(D = c("1", "2"))
## attr(y, "vars") <- names(attr(y, "lvls"))

## out <- merge_unity(x,y)

## x1 <- sptable(as.matrix(asia[, 2:5]))
## y1 <- sptable(as.matrix(asia[, 6:8]))
## y1[1:length(y1)] <- 1L
## z1 <- structure(numeric(0), class = c("sptable", "integer"))
## ny <- c("n", "y")
## attr(z1, "lvls") <- list(E = ny, X = ny, D = ny)
## attr(z1, "vars") <- names(attr(z1, "lvls"))
## class(z1) <- c("unity_table", class(z1))

## merge_unity(x1, z1)
