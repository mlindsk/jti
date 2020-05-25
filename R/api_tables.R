## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
.find_cond_configs <- function(x, pos) {
  # x  : sptable
  # pos: the position of the conditional variables

  # Should we test for variablenames containing "@"?
  skeleton <- paste(rep("@", nchar(names(x)[1])))
  .map_chr(names(x), function(s) {
    sk <- skeleton
    s_pos_val <- .map_chr(pos, function(l) substr(s, l, l))
    sk[pos] <- s_pos_val
    paste(gsub("@", "", sk), collapse = "")
  })
}

.reposition_names <- function(x, pos) {
  # x : named list
  structure(x, names =.map_chr(names(x), function(y) {
    paste(.split_chars(y)[pos], collapse = "")
  }))
}

.set_as_sptable <- function(x) {
  structure(x, class = c("sptable", class(x)))
}


## ---------------------------------------------------------
##                 EXPORTED FUNCTIONS
## ---------------------------------------------------------
#' Sparse table
#'
#' Returns a sparse contingency table for the variables in \code{x} as a vector .
#'
#' @param x Character matrix
#' @seealso \code{\link{parray}}
#' @examples
#' sptable(as.matrix(asia[, 1:2]))
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x), !is.null(colnames(x)))
  sptab <- sptab_(x)

  # TODO: Convert to envir
  
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
#' psp <- parray(sp, c("S", "T"))
#' sum(psp) # Since (S,T) have 4 configurations
#' @export
parray <- function(x, y = NULL) {
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

  if (validate) {
    nvars_x <- length(attr(x, "vars"))
    nvars_y <- length(attr(y, "vars"))
    for (name in names(x)) if (nchar(name) > nvars_x) stop("One or more names of x is not valid. See to_single_chars().")
    for (name in names(y)) if (nchar(name) > nvars_y) stop("One or more names of y is not valid. See to_single_chars().")
  }
  
  stopifnot(op %in% c("*", "/", "+", "-"))
  
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

  if (flow %ni% c("sum", "max")) stop("flow must be 'sum' or 'max'")
  
  v <- attr(p, "vars")
  if (any(is.na(match(s, v)))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- .find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  ## penv <- new.env()
  ## for (k in seq_along(p)) {
  ##   penv[[names(p)[k]]] <- as.numeric(p[k])
  ## }

  ## spt <- lapply(scf, function(e) {
  ##   if (flow == "sum") sum(unlist(mget(e, envir = penv))) else max(penv[[e]])
  ## })

  spt <- lapply(scf, function(e) {
    # TODO: Slow because we must "lookup" p[e] !!!
    if (flow == "sum") sum(p[e]) else max(p[e])
  })
  
  spt <- unlist(spt)
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable", class(spt))
  return(spt)
}

#' Print Sparse Table
#'
#' A print method for \code{sptable} and \code{slice_sptable} objects
#'
#' @param x A \code{sptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.sptable <- function(x, ...) {
  vars      <- attr(x, "vars")
  vars_cond <- attr(x, "vars_cond")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "slice")) {
    cat("", paste(paste(names(vars_cond), "= "), vars_cond, sep = "", collapse = ", "), "\n")    
  }
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  for (k in seq_along(cells)) {
    cat(cells[k], ":", x[k], "\n")
  }
  invisible(x)
}

#' @export
`[<-.sptable` <- function(x, i, value) {
  NextMethod()
}

#' @export
`[.sptable` <- function(x, i) {
  if (is.character(i) && !all(i %in% names(x))) return(0L)
  structure(.set_as_sptable(NextMethod()) , vars = attr(x, "vars"))
}

#' Array to Sparse Table
#'
#' Convert conditional probability table of array-type to a sparse representation
#'
#' @param x An \code{array}-like object
#' @param validate Logical. Check if x has proper dimnames.
#' @details Use only validate = FALSE if you are positve that the structure is correct.
#' @examples
#'
#'
#' # Convert one-dimensional array
#'
#' dmx <- list(X = c("a", "b"))
#' x   <- array(c(1, 2), dimnames = dmx)
#' as_sptable(x)
#'
#' # Convert mutli-dimensional array
#' 
#' dmy <- list(Y = c("a", "b"), Z = c("c", "d"), W = c("e", "f"))
#' y   <- array(c(1,0,3,0,0,2,1,0), c(2,2,2), dimnames = dmy)
#' as_sptable(y)
#'
#' # Convert a data frame to sptable
#'
#' as_sptable(table(asia[, 2:3]))
#' 
#'
#' @export
as_sptable <- function(x, validate = TRUE) UseMethod("as_sptable")

#' @rdname as_sptable
#' @export
as_sptable.array <- function(x, validate = TRUE)  {

  if (validate) {
    if (!is_named_list(dimnames(x))) stop("x must be a named array or matrix")
    # Test for allowed chars in the future and dont convert automatically
    dimnames(x) <- lapply(dimnames(x), function(x) possible_chars(length(x)))  
  }

  # TODO: Use an iterator instead of copying!
  cond_comb  <- expand.grid(dimnames(x), stringsAsFactors = FALSE)
  spt        <- c()
  cell_names <- c()
  
  for (k in 1:nrow(cond_comb)) {
    cc  <- unlist(cond_comb[k, ])
    val <- x[matrix(cc, 1L)]
    if (val != 0) {
      spt <- c(spt, val)
      cell_names <- c(cell_names, paste(cc, collapse = ""))
    }
  }

  spt <- structure(spt, names = cell_names)
  attr(spt, "vars") <- names(dimnames(x))
  class(spt) <- c("sptable", class(spt))
  
  return(spt)
}


#' @rdname as_sptable
#' @export
as_sptable.matrix <- as_sptable.array


#' @rdname as_sptable
#' @export
as_sptable.table <- as_sptable.array
